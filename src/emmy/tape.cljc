#_"SPDX-License-Identifier: GPL-3.0";;

(ns emmy.tape
  "Basic machinery for reverse-mode AD! Let's see how far we get."
  (:require [emmy.differential :as d]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.value :as v]))

;; ## Reverse-mode AD
;;
;; This file contains an implementation of reverse-mode AD, based on Alexey
;; Radul's implementation
;; in [dvl](https://github.com/axch/dysvunctional-language).
;;
;; Relevant notes:
;;
;; - it's an invariant that any wrapped number (in primal, tangent or partial)
;;   has an ID that is strictly less than the wrapping ID.
;;
;; - the old code has some replace-tag stuff that was superceded by the 2019
;;   paper, we need to update the reverse mode here to not worry about that for
;;   perturbation confusion.

;; The `in->partial` map in a tape cell associates
;;
;; - tape cells representing inputs to the computation whose output this tape
;;   cell represents
;; - to the partial derivatives of said output with respect to those inputs.
;;
;; The reverse phase will use these after it has computed the sensitivity to
;; said output.
;;
;; We can use Emmy's overloading to carry out the forward-mode phase for
;; differential instances, or the forward pass of reverse-mode AD.
;;
;; TODO something is going on here, that is shared in common with JAX, where the
;; AD overloads a jacobian instead of a single value.

;; TODO should a tape be a scalar? I don't know yet... what would that get us?
(derive ::tape ::f/cofunction)

;; perturbation tag, cell-id, primal, then a map of input [[TapeCell]] to the
;; partial derivative of the (primal?) with respect to that cell's input.
(defrecord TapeCell [tag id primal in->partial]
  v/IKind
  (kind [_] ::tape))

;; accessors for internal fields.
(defn tape-tag [^TapeCell tape] (.-tag tape))
(defn tape-id [^TapeCell tape] (.-id tape))
(defn tape-primal [^TapeCell tape] (.-primal tape))
(defn tape-partials [^TapeCell tape] (.-in->partial tape))

(defmethod g/zero? [::tape] [t]
  (g/zero? (tape-primal t)))

(defmethod g/one? [::tape] [t]
  (and (g/one? (tape-primal t))
       (empty? (tape-partials t))))

(defmethod g/identity? [::tape] [t]
  (and (g/identity? (tape-primal t))
       (empty? (tape-partials t))))

(defmethod g/zero-like [::tape] [_] 0)
(defmethod g/one-like [::tape] [_] 1)
(defmethod g/identity-like [::tape] [_] 1)
(defmethod g/freeze [::tape] [t]
  `[~'TapeCell
    ~(tape-tag t)
    ~(tape-id t)
    ~(g/freeze (tape-primal t))
    ~(reduce-kv (fn [acc k v]
                  (assoc acc (g/freeze k) (g/freeze v)))
                {}
                (tape-partials t))])

;; TODO do we want the "epsilon" or "tag" to be a protocol? if we did, then
;; `perturbation-of` becomes a multimethod.
;;
;; NOTE that this returns `least-gensym` currently, which is one created at
;; startup, as a default. Maybe safer to make some sentinel that's always
;; less-than, or -##Inf or something.

(defn tape? [x]
  (instance? TapeCell x))

(defn make
  "Make a `TapeCell` instance with a unique ID."
  ([tag primal]
   (make tag primal {}))
  ([tag primal partials]
   (->TapeCell tag (gensym) primal partials)))

;; ## Interaction with bundles

(defn primal-part
  "TODO THIS maybe could be a protocol if indeed the other implementations fall
  down with it. This function's behavior is how we're supposed to respond when
  FORWARD mode tries to get our primal - do not give it up to them!

  NOTE from dvl... This expects that primal is always called with the outermost
  available epsilon, and that bundles and tape cells are properly nested."
  [x _tag]
  x)

(defn tangent-part
  "TODO so weird, we ALSO only return tangent part here... oh. That is probably
  because with proper nesting, if we're on the OUTSIDE then...

  Note from DVL:

  TODO This expects that tangent is always called with the outermost available
  epsilon, and that bundles and tape cells are properly nested. (What does
  properly nested mean again?)"
  [_x _tag] 0)

(defn primal*
  "TODO this is also in ad-structures. This goes ahead and descends deep and
  returns the ACTUAL primal part of either the tape or the differential... or ID
  if it's neither of these container types."
  [thing]
  (if (tape? thing)
    (recur thing)
    thing))

(defn- reverse-primal
  "NOTE This assumes that epsilon is greater than or equal to the perturbation of
  thing, and is an epsilon associated with reverse mode."
  [x tag]
  (if (and (tape? x) (= tag (tape-tag x)))
    (tape-primal x)
    x))

(defn tag-of
  "TODO redo this and make it so it doesn't HAVE to have a tag; but right now
  we're returning 0 since it'll be less than all tags.

  NOTE this is sort of like max-order-tag; but everything is not flattened out."
  [x]
  (if (tape? x)
    (tape-tag x)
    0))

;; Now we're on to reverse-mode.dvl, off of the basic ad-structures. Let's
;; see...

(comment
  (defprotocol ITapify
    (-tapify [this tag]))

  "TODO Do this later!"
  (extend-protocol ITapify
    ,,,))

(defn tapify
  "Down to business! This should be a protocol method."
  [x tag]
  (cond
    ;; TODO more evidence that tape should be a scalar?
    (v/scalar? x)    (make tag x)
    (s/structure? x) (s/mapr #(tapify % tag) x)
    (f/function? x)  (u/illegal "Can't do this yet.")
    :else            x))

(declare compute-visiting-order*)

(defn ^:no-doc topological-sort-by-id
  "Given some `node` of type [[TapeCell]], returns a sequence of [[TapeCell]]
  instances sorted

  is the current node being visited
   `seen` is a set of seen node IDs
   `sorted` is the accumulating return value of sorted nodes."
  [node]
  (letfn [(compute-visiting-order
            [[seen sorted] node]
            (if (contains? seen (tape-id node))
              [seen sorted]
              (let [[seen sorted] (process-children
                                   (conj seen (tape-id node))
                                   sorted
                                   (tape-partials node))]
                [seen (cons node sorted)])))
          (process-children [seen sorted in->partials]
            (transduce (map key)
                       (completing compute-visiting-order)
                       [seen sorted]
                       in->partials))]
    (second
     (compute-visiting-order [#{} []] node))))

(defn reverse-phase
  "sensitivities is a map... nodes is a sequence."
  [nodes sensitivities]
  (letfn [(process [m node]
            ;; Since you're going in reverse topological sort order, when you
            ;; reach a node you know you are done updating its sensitivity.
            (let [sensitivity (get m (tape-id node))]
              (reduce-kv
               (fn [acc cell factor]
                 (let [id    (tape-id cell)
                       delta (g/* factor sensitivity)]
                   (assoc acc id (if-let [v (get acc id)]
                                   (g/+ v delta)
                                   delta))))
               m
               (tape-partials node))))]
    (reduce process sensitivities nodes)))

(defn interpret
  "TODO this has a parallel on the output. This one means, this was the input to
  the function, and now we've backpropagated all of the sensitivities. Interpret
  the input.

  Next we need to actually

  - turn the output "
  [thing tag sensitivities]
  (cond (and (tape? thing) (= tag (tape-tag thing)))
        (get sensitivities (tape-id thing) 0)

        (s/structure? thing)
        ;; TODO use mapr?
        (s/opposite
         thing (map #(interpret % tag sensitivities) thing))

        (f/function? thing) (u/illegal "function input not yet supported.")
        :else 0))

;; TODO okay, this is wrong! To handle the multivariable case we can't just walk
;; back from the output of each.
;;
;; the current way gets the structures wrong, by nesting copies of the partials
;; into the output structure.
;;
;; We need to get a copy of the OUTPUT into each entry in the output structure,
;; with orientations flipped.
(defn handle-output [thing inputs tag]
  (cond (and (tape? thing) (= tag (tape-tag thing)))
        ;; No perturbation greater than eps should be observable in
        ;; forward-phase-answer. TODO does this hold?
        (let [sensitivities
              (if (and (tape? thing) (= tag (tape-tag thing)))
                (let [sorted (topological-sort-by-id thing)]
                  (reverse-phase sorted {(tape-id thing) 1}))
                ;; f is not infinitesimally dependent on x, return
                ;; the empty sensitivity list.
                {})]
          (interpret inputs tag sensitivities))

        (vector? thing)
        (mapv #(handle-output % inputs tag) thing)

        (s/structure? thing)
        (s/mapr #(handle-output % inputs tag) thing)

        (f/function? thing)
        ;; TODO this needs to handle perturbation confusion with tag
        ;; replacement. Make something similar to extract-tangent-fn.
        (comp #(handle-output % inputs tag) thing)

        ;; TODO this needs to be a protocol to handle quaternion return values
        ;; etc...
        :else thing))

;; NOTE this is an attempt... but what do we do with the results? we have a combo of
;; types of input and output.
#_
(defn ->sensitivities [thing tag]
  (cond (and (tape? thing) (= tag (tape-tag thing)))
        ;; No perturbation greater than eps should be observable in
        ;; forward-phase-answer. TODO does this hold?
        (let [sensitivities
              (if (and (tape? thing) (= tag (tape-tag thing)))
                (let [sorted (topological-sort-by-id thing)]
                  (reverse-phase sorted {(tape-id thing) 1}))
                ;; f is not infinitesimally dependent on x, return
                ;; the empty sensitivity list.
                {})]
          {::sensitivities sensitivities
           ::node thing})

        (vector? thing)
        (mapv #(->sensitivities % tag) thing)

        (s/structure? thing)
        (s/mapr #(->sensitivities % tag) thing)

        (f/function? thing)
        ;; TODO this needs to handle perturbation confusion with tag
        ;; replacement. Make something similar to extract-tangent-fn.
        (comp #(->sensitivities % tag) thing)

        ;; TODO this needs to be a protocol to handle quaternion return values
        ;; etc...
        :else {::sensitivities {}
               ::node thing}))

(defn gradient-r
  "Returns an function that returns the gradient of the supplied fn... still quite
  simplified."
  [f]
  (fn [x]
    (let [tag    (d/fresh-tag)
          inputs (tapify x tag)
          output (f inputs)]
      (handle-output output inputs tag))))

;; ## Lifted Fns

(defn lift-1
  "As with differential, `df:dx` has to have ALREADY been lifted here."
  ([f]
   (if-let [df:dx (f :dfdx)]
     (lift-1 f df:dx)
     (u/illegal
      "No df:dx supplied for `f` or registered generically.")))
  ([f df:dx]
   (fn call [x]
     (if (tape? x)
       (let [primal (tape-primal x)]
         (make (tape-tag x)
               (call primal)
               {x (df:dx primal)}))
       (f x)))))

(defn lift-2
  ([f]
   (let [df:dx (f :dfdx)
         df:dy (f :dfdy)]
     (if (and df:dx df:dy)
       (lift-2 f df:dx df:dy)
       (u/illegal
        "No df:dx, df:dy supplied for `f` or registered generically."))))
  ([f df:dx1 df:dx2]
   (fn call [a b]
     (letfn [(operate [tag]
               (let [prim-a (reverse-primal a tag)
                     prim-b (reverse-primal b tag)
                     partial-a (if (and (tape? a) (= tag (tape-tag a)))
                                 {a (df:dx1 prim-a prim-b)}
                                 {})
                     partial-b (if (and (tape? b) (= tag (tape-tag b)))
                                 {b (df:dx2 prim-a prim-b)}
                                 {})]
                 (make tag
                       (call prim-a prim-b)
                       (into partial-a partial-b))))]
       (let [tag-a (tag-of a)
             tag-b (tag-of b)]
         (cond (and (tape? a) (not (< tag-a tag-b)))
               (operate tag-a)

               (and (tape? b) (< tag-a tag-b))
               (operate tag-b)

               :else (f a b)))))))

;; TODO add binary versions.

(defn- defunary [generic-op differential-op]
  (defmethod generic-op [::tape] [a] (differential-op a)))

(defn- defbinary [generic-op differential-op]
  (doseq [signature [[::tape ::tape]
                     [::v/scalar ::tape]
                     [::tape ::v/scalar]]]
    (defmethod generic-op signature [a b] (differential-op a b))))

#_(defbinary v/= equiv)

(defbinary g/add (lift-2 g/add))
(defunary g/negate (lift-1 g/negate))
(defbinary g/sub (lift-2 g/sub))

(let [mul  (lift-2 g/mul)]
  (defbinary g/mul mul)
  (defbinary g/dot-product mul))
(defbinary g/expt (lift-2 g/expt))

(defunary g/square (lift-1 g/square))
(defunary g/cube (lift-1 g/cube))

(defunary g/invert (lift-1 g/invert))
(defbinary g/div (lift-2 g/div))

(defunary g/negative?
  (comp g/negative? primal*))

(defunary g/infinite?
  (comp g/infinite? primal*))

(defunary g/abs
  (fn [x]
    (let [f (primal* x)
          func (cond (< f 0) (lift-1 (fn [x] x) (fn [_] -1))
                     (> f 0) (lift-1 (fn [x] x) (fn [_] 1))
                     (= f 0) (u/illegal "Derivative of g/abs undefined at zero")
                     :else (u/illegal (str "error! derivative of g/abs at" x)))]
      (func x))))

(defn- discont-at-integers [f dfdx]
  (let [f (lift-1 f (fn [_] dfdx))
        f-name (g/freeze f)]
    (fn [x]
      (if (v/integral? (primal* x))
        (u/illegal
         (str "Derivative of g/" f-name " undefined at integral points."))
        (f x)))))

(defunary g/floor
  (discont-at-integers g/floor 0))

(defunary g/ceiling
  (discont-at-integers g/ceiling 0))

(defunary g/integer-part
  (discont-at-integers g/integer-part 0))

(defunary g/fractional-part
  (discont-at-integers g/fractional-part 1))

(let [div (lift-2 g/div)]
  (defbinary g/solve-linear (fn [l r] (div r l)))
  (defbinary g/solve-linear-right div))

(defunary g/sqrt (lift-1 g/sqrt))
(defunary g/log (lift-1 g/log))
(defunary g/exp (lift-1 g/exp))

(defunary g/cos (lift-1 g/cos))
(defunary g/sin (lift-1 g/sin))
(defunary g/tan (lift-1 g/tan))
(defunary g/cot (lift-1 g/cot))
(defunary g/sec (lift-1 g/sec))
(defunary g/csc (lift-1 g/csc))

(defunary g/atan (lift-1 g/atan))
(defbinary g/atan (lift-2 g/atan))
(defunary g/asin (lift-1 g/asin))
(defunary g/acos (lift-1 g/acos))
(defunary g/acot (lift-1 g/acot))
(defunary g/asec (lift-1 g/asec))
(defunary g/acsc (lift-1 g/acsc))

(defunary g/cosh (lift-1 g/cosh))
(defunary g/sinh (lift-1 g/sinh))
(defunary g/tanh (lift-1 g/tanh))
(defunary g/sech (lift-1 g/sech))
(defunary g/coth (lift-1 g/coth))
(defunary g/csch (lift-1 g/csch))

(defunary g/acosh (lift-1 g/acosh))
(defunary g/asinh (lift-1 g/asinh))
(defunary g/atanh (lift-1 g/atanh))
(defunary g/acoth (lift-1 g/acoth))
(defunary g/asech (lift-1 g/asech))
(defunary g/acsch (lift-1 g/acsch))

(defunary g/sinc (lift-1 g/sinc))
(defunary g/sinhc (lift-1 g/sinhc))
(defunary g/tanc (lift-1 g/tanc))
(defunary g/tanhc (lift-1 g/tanhc))
