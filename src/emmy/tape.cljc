#_"SPDX-License-Identifier: GPL-3.0";;

(ns emmy.tape
  "This namespace contains an implementation of [[TapeCell]], a type that forms
  the basis for the reverse-mode automatic differentiation implementation in
  Emmy."
  (:require [emmy.differential :as d]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.matrix :as matrix]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.value :as v]))

;; ## Reverse-mode Automatic Differentiation
;;
;; > The AD implementation developed in this namespace is based on Alexey
;; > Radul's [implementation in
;; > dvl](https://github.com/axch/dysvunctional-language).
;;
;; This namespace develops an implementation of "reverse-mode" automatic
;; differentation, in contrast with the forward mode AD implementation
;; in [[emmy.differential]]. These two are closely related, and the
;; implementations could merge once I get around to reading and
;; implementing [the YOLO paper](https://arxiv.org/abs/2204.10923).
;;
;; The core idea of forward-mode AD is that we can use the chain rule to
;; mechanically build up a partial derivative by wrapping one of the dependent
;; variables in a "dual number" and accumulating the derivative as the
;; calculation progresses.
;;
;; For functions with one input and many outputs, this style computes the full
;; partial derivative in one pass.
;;
;; The downside of forward-mode AD is that each partial derivative requires
;; running the full computation for each independent input variable. For
;; functions with many inputs and a single output (like the Langrangian), this
;; linear scaling is not great.

;; "Reverse-mode" AD is an alternative way of building up all of the partial
;; derivatives for such a function in a single pass.
;;
;; > NOTE I plan on fleshing out a full, worked example of the how the chain
;; > rule works in reverse-mode... but please accept this sketch for now.
;;
;; The approach works like this:
;;
;; First, wrap all inputs to the function in an instance of a type called
;; a [[TapeCell]]. This type holds
;;
;;   - a "tag", used as in [[emmy.differential]] to prevent confusion
;;     between [[TapeCell]] instances created for different nested runs of
;;     differentation
;;   - a unique ID, used to de-duplicate work in the reverse pass
;;   - the "primal value" wrapped by this [[TapeCell]] instance
;;   - A map of the [[TapeCell]]'s inputs => the partial derivative of the
;;     primal value with respect to that input
;;
;; Pass the augmented inputs into the original function, which should be built
;; out of primitives that are overloaded to deal with [[TapeCell]] instances.
;;
;; A primitive `f` of `n` arguments, given [[TapeCell]] inputs, should return a
;; new [[TapeCell]] of the form:
;;
;; ```clojure
;; {:tag <input tag>
;;  :id <fresh ID>
;;  :primal (f a b c ...)
;;  :in->partials {<tape-a> <(((partial 0) f) a b c ...)>
;;                 <tape-b> <(((partial 1) f) a b c ...)>
;;                 <tape-c> <(((partial 2) f) a b c ...)>
;;                 ...}
;; }
;; ```
;;
;; The output will be either a final [[TapeCell]] instance, or some structure
;; composed of [[TapeCell]]s. (Any other kind of output means that the output
;; doesn't depend on the inputs, so the derivative of that entry should be 0.)
;;
;; Let's stick with the case of a single [[TapeCell]] output. The [[TapeCell]]
;; represents a directed acyclic graph of the computation. The edges point from
;; each cell down into its `:in->partials` entries, and point to the nodes that
;; affected the current node.
;;
;; Nodes that are re-used can show up in many `:in->partials` entries, so we
;; start by sorting the [[TapeCell]]s of the graph in topological order, with
;; the output node at the root. This gives us a sequence of nodes with the
;; property that when we encounter a node, no node we see afterward can affect
;; it.
;;
;; To perform the "reverse pass" of AD, traverse this list with an accumulator
;; of the form `{<tapecell-id> <partial derivative>}` and for each [[TapeCell]],
;; increment each input's entry by the current node's entry multiplied by the
;; partial derivative of the current node with respect to that input.
;;
;; The result of the reverse pass is a completed map of the partial derivatives
;; of every value, beginning and intermediate, seen in the computation. In the
;; multivariable output case, you'll have a structure of these maps.
;;
;; To complete the derivative, walk the augmented input:
;;
;; - for each [[TapeCell]], replace the [[TapeCell]] with its entry in this map
;; - for the multivariable output case, walk the output structure and replace
;;   each map with the value for the input entry's [[TapeCell]].
;;
;; ### Notes for Implementation
;;
;; - I don't think that the implementation here will play well when nested with
;;   forward-mode AD. In Alexey Radul's DVL implementation, [[TapeCell]]
;;   and [[emmy.differential/Differential]] are both implemented in the same
;;   namespace and more tightly bound. We'll have to do that here too.
;;
;; - NOTE: it's an invariant that any wrapped number (in primal, tangent or
;;   partial) has an ID that is strictly less than the wrapping ID. Test for
;;   this, and describe why it matters.
;;
;; - This implementation doesn't handle perturbation confusion properly for
;; - functions, and I suspect that we'll see Alexey's Amazing Bug. In another
;; - pass I want to carefully test this and make sure it works well.

;; ## TapeCell Implementation
;;
;; A [[TapeCell]] will respond to [[v/kind]] with `::tape`. To
;; allow [[TapeCell]] instances to work in any place that real numbers or
;; symbolic argument work, make `::tape` derive from `::v/scalar`:

(derive ::tape ::v/scalar)

;; Here's the [[TapeCell]] type with the fields described above.

(defrecord TapeCell [tag id primal in->partial]
  v/IKind
  (kind [_] ::tape)

  v/Numerical
  (numerical? [_]
    ;; TODO do we care if in->partial is empty? Same question below for `one?`
    ;; etc implementations.
    (and (v/numerical? primal)
         (empty? in->partial))))

;; ## Non-generic API

(defn tape?
  "Returns true if the supplied object is an instance of [[TapeCell]], false
  otherwise."
  [x]
  (instance? TapeCell x))

(defn ^:no-doc fresh-id
  "Returns a new, unique ID for use with a new [[TapeCell]]."
  []
  (gensym))

(defn make
  "Returns a [[TapeCell]] instance with the supplied `tag` and `primal` values.

  Optionally accepts `partials`, a map of input cell => the partial derivative
  of the output with respect to each input (defaults to `{}`)."
  ([tag primal]
   (->TapeCell tag (fresh-id) primal {}))
  ([tag primal partials]
   (->TapeCell tag (fresh-id) primal partials)))

;; TODO making [[tapify]] extensible is the key to differentiating things like
;; quaternion-valued functions. Forward-mode handles this differently, since we
;; can't populate all of the input values in a single shot.

(defn tapify
  "Given a scalar input `x`, wraps the input in a fresh [[TapeCell]] instance
  tagged with `tag`.

  Given a structural `x` instance, returns an identically-shaped structure with
  all leaves recursively replaced by [[TapeCell]] instances."
  [x tag]
  (cond
    (v/scalar? x)    (make tag x)
    (s/structure? x) (s/mapr #(tapify % tag) x)
    (f/function? x)  (u/illegal "Function input not yet supported.")
    :else            x))

;; ### Accessors

(defn tape-tag
  "Returns the `-tag` field of the supplied [[TapeCell]] object. Errors if any
  other type is supplied.

  Tags are used to distinguish multiple, overlapping runs of reverse-mode AD, so
  that [[TapeCell]] instances created for each run don't clash."
  [^TapeCell tape]
  (.-tag tape))

(defn tag-of
  "More permissive version of [[tape-tag]] that returns ##-Inf, the 'least
  possible tag', when passed a non-[[TapeCell]] instance.

  TODO this will need to be extended to
  handle [[emmy.differential/Differential]] instances when these namespaces
  merge."
  [x]
  (if (tape? x)
    (tape-tag x)
    0))

(defn tape-id
  "Returns the `-id` field of the supplied [[TapeCell]] object. Errors if any
  other type is supplied.

  IDs are used in the reverse pass of automatic differentiation to prevent
  duplicating work for [[TapeCell]]s used as input to multiple
  other [[TapeCell]]s."
  [^TapeCell tape]
  (.-id tape))

(defn tape-primal
  "Given a [[TapeCell]], returns the `-primal` field of the supplied [[TapeCell]]
  object. For all other types, acts as identity.

  If the optional `tag` argument is supplied, only returns `-primal`
  if `(tape-tag x)` matches `tag`, else acts as identity."
  ([x]
   (if (tape? x)
     (.-primal ^TapeCell x)
     x))
  ([x tag]
   (if (and (tape? x) (= tag (tape-tag x)))
     (.-primal ^TapeCell x)
     x)))

(defn tape-partials
  "Returns the `in->partials` map of the supplied [[TapeCell]] object. Errors if
  any other type is supplied.

  This map maps from each of the `tape`'s inputs to the partial derivative
  of `(tape-primal tape)` with respect to the input."
  [^TapeCell tape]
  (.-in->partial tape))

(defn deep-primal
  "Version of [[tape-primal]] that will descend recursively into any [[TapeCell]]
  instance returned by [[tape-primal]] until encountering a non-[[TapeCell]].

  Given a non-[[TapeCell]], acts as identity."
  [v]
  (if (tape? v)
    (recur (tape-primal v))
    v))

;; ### Reverse-pass support

(defn ^:no-doc topological-sort-by-id
  "Given a `node` of type [[TapeCell]] (representing the root of a computation's
  directed acyclic dependency graph), returns a sequence of [[TapeCell]]
  instances sorted in topological order, starting with `node`."
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

;; [[reverse-phase]] will generate a map of node ID => partial derivative,
;; called a "sensitivity" while it's being accumulated. For functions that
;; return, say, a map of key => perturbed value, we need to run the reverse
;; phase for every value; rather than store a new map at each slot, we create a
;; new [[Completed]] type to distinguish nested maps from sensitivity maps.
;;
(defrecord Completed [v->partial])

(defn ^:no-doc reverse-phase
  "Accepts a [[TapeCell]] `root` representing the final value, or output, of a
  reverse-mode derivative computation, and returns a [[Completed]] instance
  wrapping a map of

  - each intermediate value seen in the computation to
  - the partial derivative of the output with respect to that value."
  [root]
  (let [nodes   (topological-sort-by-id root)
        acc     {(tape-id root) 1}
        process (fn [m node]
                  ;; Since we're processing in topological sort order, when we
                  ;; reach a node we know we're done updating its
                  ;; sensitivity (it can't be affected by nodes that come
                  ;; after).
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
    (->Completed
     (reduce process acc nodes))))

;; [[reverse-phase]] above operates on a single [[TapeCell]]. For structured
;; outputs, we need to walk the output until we hit a [[TapeCell]] instance and
;; call [[reverse-phase]] for each. This will result in an output-shaped
;; structure of [[Completed]] instances.
;;
;; TODO [[->partials]] really should depend on `fmap`, as should so many other
;; things in the library. Without this we can't generically support output
;; values like maps or quaternions. [[emmy.differential/extract-tangent]] does
;; this for forward-mode, I believe.

(defn ^:no-doc ->partials
  "Given some possibly-structured `output` of the forward pass and a `tag`,
  returns either

  - a [[Completed]] instance wrapping a map of intermediate value => partial
    derivative, or
  - the same structure as `output` will leaves replaced with [[Completed]]
    instances

  Pass the result to [[extract]] along with the ID of some input node to obtain
  the partial derivative with respect to that ID."
  [output tag]
  (cond (and (tape? output) (= tag (tape-tag output)))
        (reverse-phase output)

        (vector? output)
        (mapv #(->partials % tag) output)

        (s/structure? output)
        (s/mapr #(->partials % tag) output)

        (f/function? output)

        ;; TODO this needs to handle perturbation confusion with tag
        ;; replacement. Make something similar to extract-tangent-fn and test
        ;; this so we don't allow an amazing bug.
        (comp #(->partials % tag) output)

        :else (->Completed {})))

;; Unfortunately we require two passes over the output structure. The first one
;; generates the maps of partials, and the second pass extracts the partial we
;; care about.
;;
;; If we only have a single input, we could get away with a single pass and
;; combine these. But for multiple inputs, [[extract]] will be called multiple
;; times, one for each input.

(defn ^:no-doc extract
  "Given

  - an `output` [[Completed]] instance or structure of completed instances
  - the `id` of some variable

  returns a value of the same shape as `output` with all [[Completed]] instances
  replaced by the partial derivative associated with `id`."
  [output id]
  (cond (instance? Completed output)
        (get (:v->partial output) id 0)

        (vector? output)
        (mapv #(extract % id) output)

        (s/structure? output)
        (s/mapr #(extract % id) output)

        (f/function? output)
        ;; TODO this needs to handle perturbation confusion with tag
        ;; replacement. Make something similar to extract-tangent-fn.
        (comp #(extract % id) output)

        :else 0))

;; TODO note that [[interpret]] and [[tapify]] both need to become generic on
;; input-walking, and [[extract]] and [[->partials]] need to be generic on
;; output-walking.
;;
;; I am sure we are going to need to glom on to the [[replace-tag]] machinery as
;; well. [[emmy.differential/extract-tangent]] is similar to what we want. Maybe
;; we can share?

(defn ^:no-doc interpret
  "Given

  - a perturbed input, either a [[TapeCell]] or structure of [[TapeCell]]s
  - an `output` [[Completed]] instance or structure of completed instances
  - the `tag` for the current run of differentiation

  Returns a value with the same shape as `input`, but with each [[TapeCell]]
  leaf replaced by copies of `output` representing the partial derivative of the
  computation at that leaf."
  [input output tag]
  (cond (and (tape? input) (= tag (tape-tag input)))
        (extract output (tape-id input))

        (s/structure? input)
        ;; TODO if we don't care about flipping for gradient, then use mapr.
        (s/opposite input (mapv #(interpret % output tag) input))

        (f/function? input) (u/illegal "function input not yet supported.")

        :else 0))

;; ## Gradient

(defn gradient
  "Given some differentiable function `f`, returns a function whose value at some
  point can multiply an increment in the arguments to produce the best linear
  estimate of the increment in the function value.

  For scalar-valued functions, [[gradient]] computes a derivative. For
  vector-valued functions, [[gradient]] computes
  the [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
  of `f`.

  For numerical differentiation, see [[emmy.numerical.derivative/D-numeric]].

  NOTE: `f` must be built out of generic operations that know how to
  handle [[emmy.tape/TapeCell]] inputs in addition to any types that a
  normal `(f x)` call would present. This restriction does _not_ apply to
  operations like putting `x` into a container or destructuring; just primitive
  function calls."
  [f]
  (fn
    ([] 0)
    ([x]
     (let [tag       (d/fresh-tag)
           inputs    (tapify x tag)
           output    (f inputs)
           completed (->partials output tag)]
       (interpret inputs completed tag)))
    ([x & more]
     ((gradient (fn [args]
                  (apply f args)))
      (matrix/seq-> (cons x more))))))

;; ## Lifted Functions
;;
;; NOTE these next two functions are similar to the functions
;; in [[emmy.differential]]; both of these should be merged and install methods
;; that can handle the interaction between [[TapeCell]]
;; and [[emmy.differential/Differential]] instances.
;;
;; To support reverse-mode automatic differentiation, When a unary or binary
;; function `f` encounters a [[TapeCell]] `x` (and `y` in the binary case) it
;; needs to return a new [[TapeCell]] with:
;;
;; - the same tag
;; - a fresh, unique ID
;; - a primal value `(f x)` (or `(f x y)`)

;; - a map of each input to the partial of `f` with respect to that input.
;;   So, `{x ((D f) x)}` in the unary case, and
;;
;; ```clojure
;; {x (((partial 0) f) x y)
;;  y (((partial 1) f) x y)}
;; ````
;;
;;  in the binary case.
;;
;; The partial derivative implementations are passed in directly or retrieved
;; from the generic implementation using the same method as in
;; the [[emmy.differential]] versions, hinting again that we should unify these.

(defn lift-1
  "Given:

  - some unary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument

  Returns a new unary function that operates on both the original type of `f`
  and [[TapeCell]] instances.

  If called without `df:dx`, `df:dx` defaults to `(f :dfdx)`; this will return
  the derivative registered to a generic function defined
  with [[emmy.util.def/defgeneric]].

  NOTE: `df:dx` has to ALREADY be able to handle [[TapeCell]] instances. The
  best way to accomplish this is by building `df:dx` out of already-lifted
  functions, and declaring them by forward reference if you need to."
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
  "Given:

  - some binary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument
  - a function `df:dy`, similar to `df:dx` for the second arg

  Returns a new binary function that operates on both the original type of `f`
  and [[TapeCell]] instances.

  NOTE: `df:dx` and `df:dy` have to ALREADY be able to handle [[TapeCell]]
  instances. The best way to accomplish this is by building `df:dx` and `df:dy`
  out of already-lifted functions, and declaring them by forward reference if
  you need to."
  ([f]
   (let [df:dx (f :dfdx)
         df:dy (f :dfdy)]
     (if (and df:dx df:dy)
       (lift-2 f df:dx df:dy)
       (u/illegal
        "No df:dx, df:dy supplied for `f` or registered generically."))))
  ([f df:dx df:dy]
   (fn call [x y]
     (letfn [(operate [tag]
               (let [primal-x  (tape-primal x tag)
                     primal-y  (tape-primal y tag)
                     partial-x (if (and (tape? x) (= tag (tape-tag x)))
                                 {x (df:dx primal-x primal-y)}
                                 {})
                     partial-y (if (and (tape? y) (= tag (tape-tag y)))
                                 {y (df:dy primal-x primal-y)}
                                 {})]
                 (make tag
                       (call primal-x primal-y)
                       (into partial-x partial-y))))]
       (let [tag-x (tag-of x)
             tag-y (tag-of y)]
         (cond (and (tape? x) (not (< tag-x tag-y)))
               (operate tag-x)

               (and (tape? y) (< tag-x tag-y))
               (operate tag-y)

               :else (f x y)))))))

;; ## Generic Method Installation
;;
;; Armed with [[lift-1]] and [[lift-2]], we can install [[TapeCell]] into
;; the Emmy generic arithmetic system.

(defn- defunary
  "Given:

  - a generic unary multimethod `generic-op`
  - optionally, a corresponding single-arity lifted function
    `differential-op` (defaults to `(lift-1 generic-op)`)

  installs an appropriate unary implementation of `generic-op` for `::tape`
  instances."
  ([generic-op]
   (defunary generic-op (lift-1 generic-op)))
  ([generic-op differential-op]
   (defmethod generic-op [::tape] [a] (differential-op a))))

(defn- defbinary
  "Given:

  - a generic binary multimethod `generic-op`
  - optionally, a corresponding 2-arity lifted function
    `differential-op` (defaults to `(lift-2 generic-op)`)

  installs an appropriate binary implementation of `generic-op` between `:tape`
  and `::v/scalar` instances."
  ([generic-op]
   (defbinary generic-op (lift-2 generic-op)))
  ([generic-op differential-op]
   (doseq [signature [[::tape ::tape]
                      [::v/scalar ::tape]
                      [::tape ::v/scalar]]]
     (defmethod generic-op signature [a b] (differential-op a b)))))

(defn ^:no-doc by-primal
  "Given some unary or binary function `f`, returns an augmented `f` that acts on
  the primal entries of any [[TapeCell]] arguments encounted, irrespective of
  tag.

  Given a [[TapeCell]] with a [[TapeCell]] in its [[primal-part]], the returned
  `f` will recursively descend until it hits a non-[[TapeCell]]."
  [f]
  (fn
    ([x] (f (deep-primal x)))
    ([x y] (f (deep-primal x)
              (deep-primal y)))))

(defbinary g/add)
(defunary g/negate)
(defbinary g/sub)

(let [mul (lift-2 g/mul)]
  (defbinary g/mul mul)
  (defbinary g/dot-product mul))
(defbinary g/expt)

(defunary g/square)
(defunary g/cube)

(defunary g/invert)
(defbinary g/div)

(defunary g/abs
  (fn [x]
    (let [f (deep-primal x)
          func (cond (< f 0) (lift-1 (fn [x] x) (fn [_] -1))
                     (> f 0) (lift-1 (fn [x] x) (fn [_] 1))
                     (= f 0) (u/illegal "Derivative of g/abs undefined at zero")
                     :else (u/illegal (str "error! derivative of g/abs at" x)))]
      (func x))))

(defn- discont-at-integers [f dfdx]
  (let [f (lift-1 f (fn [_] dfdx))
        f-name (g/freeze f)]
    (fn [x]
      (if (v/integral? (deep-primal x))
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

(defunary g/sqrt)
(defunary g/log)
(defunary g/exp)

(defunary g/cos)
(defunary g/sin)
(defunary g/tan)
(defunary g/cot)
(defunary g/sec)
(defunary g/csc)

(defunary g/atan)
(defbinary g/atan)
(defunary g/asin)
(defunary g/acos)
(defunary g/acot)
(defunary g/asec)
(defunary g/acsc)

(defunary g/cosh)
(defunary g/sinh)
(defunary g/tanh)
(defunary g/sech)
(defunary g/coth)
(defunary g/csch)

(defunary g/acosh)
(defunary g/asinh)
(defunary g/atanh)
(defunary g/acoth)
(defunary g/asech)
(defunary g/acsch)

(defunary g/sinc)
(defunary g/sinhc)
(defunary g/tanc)
(defunary g/tanhc)

;; Non-differentiable generic operations

(defbinary v/= (by-primal v/=))
(defunary g/one? (by-primal g/one?))
(defunary g/identity? (by-primal g/identity?))
(defunary g/negative? (by-primal g/negative?))
(defunary g/infinite? (by-primal g/infinite?))

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
