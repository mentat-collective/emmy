#_"SPDX-License-Identifier: GPL-3.0";;

(ns emmy.tape
  "Basic machinery for reverse-mode AD! Let's see how far we get."
  (:require [emmy.differential :as d]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.value :as v]))

;; Start with a port of the DVL implementation.

(derive ::tape ::f/cofunction)

(deftype TapeCell [tag id primal in->partial]
  v/Value
  (zero? [_]
    (v/zero? primal))

  (one? [_]
    (and (v/one? primal)
         (empty? in->partial)))

  (identity? [_]
    (and (v/identity? primal)
         (empty? in->partial)))

  (zero-like [_] 0)
  (one-like [_] 1)
  (identity-like [_] 1)
  (freeze [_]
    `[~'TapeCell ~tag ~id ~(v/freeze primal)
      ~(mapv (fn [[k v]] [(v/freeze k) (v/freeze v)])
             in->partial)])
  (exact? [_] false)

  (kind [_] ::tape))

;; These come from stdlib/ad-structures.dvl.

(defn tape-tag [^TapeCell tape] (.-tag tape))
(defn tape-id [^TapeCell tape] (.-id tape))
(defn tape-primal [^TapeCell tape] (.-primal tape))
(defn tape-partials [^TapeCell tape] (.-in->partial tape))

;; TODO do we want the "epsilon" or "tag" to be a protocol? no way...

(defn tape? [x]
  (instance? TapeCell x))

(defn make
  "Make a `TapeCell` instance with a unique ID."
  ([tag primal]
   (make tag primal []))
  ([tag primal partials]
   (->TapeCell tag (gensym) primal partials)))

(defn primal-part
  "TODO THIS maybe could be a protocol if indeed the other implementations fall
  down with it. This is a flag about how we're supposed to respond when FORWARD
  mode tries to get our primal - do not give it up!!

  NOTE from dvl... This expects that primal is always called with the outermost
  available epsilon, and that bundles and tape cells are properly nested."
  [x _tag]
  x)

(defn- reverse-primal
  "This assumes that epsilon is greater than or equal to the perturbation of
  thing, and is an epsilon associated with reverse mode."
  [x tag]
  (if (and (tape? x) (= tag (tape-tag x)))
    (tape-primal x)
    x))

(defn primal*
  "TODO this is also in ad-structures. This goes ahead and descends deep and
  returns the ACTUAL primal part of either the tape or the differential... or ID
  if it's neither of these container types."
  [thing]
  (if (tape? thing)
    (primal* thing)
    thing))

;; TODO BOOM we actually... do we need to be more careful about extracting the
;; appropriate primal part? I think the deal is that we get primals out of
;; bundles, and just straight-up return them if they are tapes.

(defn tangent-part
  "TODO so weird, we ALSO only return tangent part here... oh. That is probably
  because with proper nesting, if we're on the OUTSIDE then..."
  [_x _tag] 0)

(defn tag-of
  "TODO redo this and make it so it doesn't HAVE to have a tag; but right now
  we're returning 0 since it'll be less than all tags.

  NOTE this is sort of like max-order-tag; but everything is not flattened out."
  [x]
  (if (tape? x)
    (tape-tag x)
    0))

(comment
  ;; ALSO in primal-part we have this tag hiding thing. So looks like that does
  ;; not have to change.
  (hide-gensym-in-procedure epsilon
                            (lambda (x)
                                    (primal epsilon (thing x)))))

(defprotocol ITapify
  (-tapify [this tag]))

#_
(comment
  "TODO Do this later!"
  (extend-protocol ITapify ...))

(defn tapify
  "Down to business! This should be a protocol method."
  [x tag]
  (cond (v/scalar? x)   (make tag x [])

        (s/structure? x) (s/mapr #(tapify % tag) x)
        (f/function? x)  (u/illegal "Can't do this yet.")

        :else x))

(defn reverse-phase
  "sensitivities is a map... nodes is a sequence."
  [nodes sensitivities]
  (if (empty? nodes)
    sensitivities
    ;; Since you're going in topological sort order, when you reach
    ;; a node you know you are done updating its sensitivity.
    (let [sensitivity (get sensitivities (tape-id (first nodes)))]
      (loop [sensitivities sensitivities
             partials      (tape-partials (first nodes))]
        (if (empty? partials)
          (reverse-phase (rest nodes) sensitivities)
          (let [[[partial-cell partial-factor] & rest] partials
                delta (g/* partial-factor sensitivity)]
            (recur (update sensitivities
                           (tape-id partial-cell)
                           (fn [v]
                             (if v (g/+ v delta) delta)))
                   rest)))))))

(declare compute-visiting-order*)

(defn compute-visiting-order [node seen sorted]
  (if (contains? seen (tape-id node))
    [seen sorted]
    (let [[seen sorted] (compute-visiting-order*
                         (map first (tape-partials node))
                         (conj seen (tape-id node))
                         sorted)]
      [seen (cons node sorted)])))

(defn compute-visiting-order* [nodes seen sorted]
  (if (empty? nodes)
    [seen sorted]
    (let [[seen sorted] (compute-visiting-order
                         (first nodes) seen sorted)]
      (compute-visiting-order* (rest nodes) seen sorted))))

(defn interpret [thing tag sensitivities]
  (cond (and (tape? thing)
             (= tag (tape-tag thing)))
        (get sensitivities (tape-id thing))

        (s/structure? thing)
        (s/opposite
         thing (map #(interpret % tag sensitivities) thing))

        (f/function? thing)  (u/illegal "function return not yet supported.")
        :else 0))

(defn gradient
  "Returns an function that returns the gradient of the supplied fn... still quite
  simplified."
  [f]
  (fn [x]
    (let [tag    (d/fresh-tag)
          inputs (tapify x tag)
          fwd    (f inputs)
          sensitivities (if (tape? fwd)
                          (if (= tag (tape-tag fwd))
                            (let [[_seen sorted] (compute-visiting-order fwd #{} [])]
                              (reverse-phase sorted {(tape-id fwd) 1}))
                            ;; f is not infinitesimally dependent on x, return
                            ;; the empty sensitivity list.
                            {})
                          {})]
      (interpret inputs tag sensitivities))))

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
               [[x (df:dx primal)]]))
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
                     partial-a (if (and (tape? a)
                                        (= tag (tape-tag a)))
                                 [[a (df:dx1 prim-a prim-b)]]
                                 [])
                     partial-b (if (and (tape? b)
                                        (= tag (tape-tag b)))
                                 [[b (df:dx2 prim-a prim-b)]]
                                 [])]
                 (make tag
                       (call prim-a prim-b)
                       (concat partial-a partial-b))))]
       (let [tag-a (tag-of a)
             tag-b (tag-of b)]
         (cond (and (tape? a) (not (< tag-a tag-b)))
               (operate tag-a)

               (and (tape? b) (< tag-a tag-b))
               (operate tag-b)

               :else (f a b)))))))

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
        f-name (v/freeze f)]
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
