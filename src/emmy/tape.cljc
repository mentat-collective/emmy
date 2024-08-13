#_"SPDX-License-Identifier: GPL-3.0";;

(ns emmy.tape
  "This namespace contains an implementation of [[TapeCell]], a type that forms
  the basis for the reverse-mode automatic differentiation implementation in
  Emmy."
  (:refer-clojure :exclude [compare zero?])
  (:require #?(:clj [clojure.pprint :as pprint])
            [emmy.dual :as d]
            [emmy.function :as f]
            [emmy.generic :as g]
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
;; in [[emmy.dual]].
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
;; derivatives for such a function in a single pass. Rather than accumulating
;; the chain rule values forward, we build up a tree representing the full
;; computation and then walk backward along each path in the computation graph
;; from a single output variable back to every input, multiplying the chain rule
;; entries in reverse order as we go.
;;
;; > NOTE I plan on fleshing out a full, worked example of the how the chain
;; > rule works in reverse-mode... but please accept this sketch for now.
;;
;; The approach works like this:
;;
;; First, wrap all inputs to the function in an instance of a type called
;; a [[TapeCell]]. This type holds
;;
;;   - a "tag", used as in [[emmy.dual]] to prevent confusion
;;     between [[TapeCell]] instances created for different nested runs of
;;     differentation
;;   - a unique ID, used to de-duplicate work in the reverse pass
;;   - the "primal value" wrapped by this [[TapeCell]] instance
;;   - A vector of pairs of each [[TapeCell]] input paired with the partial
;;     derivative of the primal value with respect to that input
;;
;; Next, pass the augmented inputs into the original function, which should be
;; built out of primitives that are overloaded to deal with [[TapeCell]]
;; instances.
;;
;; Given [[TapeCell]] inputs, a primitive `f` of `n` arguments, will return a
;; new [[TapeCell]] of the form:
;;
;; ```clojure
;; {:tag <input tag>
;;  :primal (f a b c ...)
;;  :id <fresh ID>
;;  :in->partial [[<tape-a> <(((partial 0) f) a b c ...)>]
;;                [<tape-b> <(((partial 1) f) a b c ...)>]
;;                [<tape-c> <(((partial 2) f) a b c ...)]>
;;                ...]
;; }
;; ```
;;
;; The output will be either a final [[TapeCell]] instance, or some structure
;; composed of [[TapeCell]]s. (Any other kind of output means that the output
;; doesn't depend on the inputs, so the derivative of `f` should be 0.)
;;
;; Let's stick with the case of a single [[TapeCell]] output. The [[TapeCell]]
;; represents a directed acyclic graph of the computation. The edges point from
;; each cell down into its `:in->partial` entries, and point to the nodes that
;; affected the current node.
;;
;; Nodes that are re-used can show up in many `:in->partial` entries, so we
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
;; ## TapeCell Implementation
;;
;; A [[TapeCell]] will respond to [[v/kind]] with `::tape`. To
;; allow [[TapeCell]] instances to work in any place that real numbers or
;; symbolic argument work, make `::tape` derive from `::v/scalar`:

(derive ::tape ::v/scalar)

;; Here's the [[TapeCell]] type with the fields described above.

(declare compare reverse-phase)

(deftype TapeCell [tag id primal in->partial]
  v/IKind
  (kind [_] ::tape)

  d/IPerturbed
  (replace-tag [this old new]
    (if (= old tag)
      (TapeCell. new id primal in->partial)
      this))

  ;; If called with [[emmy.dual/FORWARD-MODE]], a [[TapeCell]] should be treated
  ;; like a scalar, with a 0-valued tangent component.
  ;;
  ;; Else, only respond with [[reverse-phase]] if the tags match.
  (extract-tangent [this t mode]
    (cond (= mode d/FORWARD-MODE) 0
          (= t tag)               (reverse-phase this)
          :else                   d/REVERSE-EMPTY))

  (extract-id [_ _] 0)

  ;; A [[TapeCell]] has to respond `false` to all [[emmy.value/numerical?]]
  ;; inquiries; if we didn't do this, then [[emmy.generic/*]] and friends would
  ;; attempt to apply shortcuts like `(* x <tape-with-1>) => x`, stripping off
  ;; the [[TapeCell]] identity of the result and ruining the derivative.
  v/Numerical
  (numerical? [_] false)

  Object
  ;; Comparing [[TapeCell]] objects using `equals` defaults to [[equiv]], which
  ;; compares instances only using their non-tagged ('finite') components. If
  ;; you want to compare two instances using `tag` and `in->partial`,
  ;; See [[eq]].
  #?(:cljs (valueOf [_] (.valueOf primal)))
  (toString [_]
    (str "#emmy.tape.TapeCell"
         {:tag         tag
          :id          id
          :primal      primal
          :in->partial in->partial}))

  #?@(:clj
      ;; The motivation for this override is subtle. To participate in control
      ;; flow operations, like comparison with both [[TapeCell]] and
      ;; non-[[TapeCell]] instances, [[TapeCell]] instances should compare using
      ;; ONLY their primal terms. This means that comparison will ignore any
      ;; difference in `in->partial`.
      [Comparable
       (compareTo [a b] (compare a b))]

      :cljs
      [IComparable
       (-compare [a b]  (compare a b))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer (.toString x)))]))

#?(:clj
   (defmethod print-method TapeCell
     [^TapeCell t ^java.io.Writer w]
     (.write w (.toString t))))

#?(:clj
   ;; When pretty-printing we want a properly formatted map representation, so we
   ;; ditch the `emmy.tape.TapeCell` prefix that `toString` prints and act more
   ;; like a `defrecord` would.
   ;;
   ;; NOTE that this override only works in Clojure. In cljs, `simple-dispatch`
   ;; isn't extensible.
   (do (declare tapecell->map)
       (defmethod pprint/simple-dispatch TapeCell [t]
         (pprint/simple-dispatch
          (tapecell->map t)))))

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

  Optionally accepts `partials`, a vector of pairs of the form

  ```
  [<input cell> <partial>]
  ```

  where `<partial>` is the partial derivative of the output with respect to each
  input (defaults to `[]`)."
  ([tag primal]
   (->TapeCell tag (fresh-id) primal []))
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

(defn tape-id
  "Returns the `-id` field of the supplied [[TapeCell]] object. Errors if any
  other type is supplied.

  IDs are used in the reverse pass of automatic differentiation to prevent
  duplicating work for [[TapeCell]]s used as input to multiple
  other [[TapeCell]]s."
  [^TapeCell tape]
  (.-id tape))

(defn tape-primal
  "Given a [[TapeCell]], returns the `primal` field of the supplied [[TapeCell]]
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
  "Returns the `in->partial` vector of the supplied [[TapeCell]] object. Errors
  if any other type is supplied.

  This vector holds pairs with these two entries:

  - some input to `tape`
  - the partial derivative of `tape`'s [[tape-primal]] with respect to that
    input"
  [^TapeCell tape]
  (.-in->partial tape))

(defn ^:no-doc tapecell->map
  "Given a [[TapeCell]] `t`, returns a map of the form

  ```clojure
  {:tag         (tape-tag t)
   :id          (tape-id t)
   :primal      (tape-primal t)
   :in->partial (tape-partials t)}
  ```"
  [^TapeCell t]
  {:tag         (.-tag t)
   :id          (.-id t)
   :primal      (.-primal t)
   :in->partial (.-in->partial t)})

;; ### Comparison, Control Flow
;;
;; Functions like `=`, `<` and friends don't have derivatives; instead, they're
;; used for control flow inside of Clojure functions. To play nicely with these
;; functions, the [[TapeCell]] API exposes a number of methods for comparing
;; numbers on ONLY their finite parts.
;;
;; Why? If `x` is a [[TapeCell]] instance, `(< x 10)` needs to return true
;; whenever a non-[[TapeCell]] `x` would return true. To make this work, these
;; operations look only at the [[tape-primal]].
;;
;; If you need full comparison:
;;
;; - [[clojure.core/=]] compares instances with instance equality
;; - [[eq]] compares [[TapeCell]] instances by tag, primal and partials
;;
;; while:
;;
;; - [[g/zero?]], [[g/one?]], [[g/identity?]], [[equiv]] and [[compare]] only
;;   examine the [[tape-primal]] of either side.

(defn eq
  "For non-[[TapeCell]]s, identical to [[emmy.value/=]].
  For [[TapeCell]] instances, equality acts on [[tape-tag]]
  and [[tape-partials]] too.

  If you want to ignore the tangent components, use [[equiv]]."
  ([_] true)
  ([a b]
   (let [ta? (tape? a) tb? (tape? b)]
     (cond (and ta? tb?)
           (let [a ^TapeCell a
                 b ^TapeCell b]
             (and (v/= (.-primal a) (.-primal b))
                  (v/= (.-tag a) (.-tag b))
                  (v/= (.-in->partial a) (.-in->partial b))))

           ta? (and (empty? (tape-partials a))
                    (v/= (tape-primal a) b))

           tb? (and (empty? (tape-partials b))
                    (v/= (tape-primal b) a))

           :else (v/= a b))))
  ([a b & more]
   (if (eq a b)
     (if (next more)
       (recur b (first more) (next more))
       (eq b (first more)))
     false)))

(defn equiv
  "Returns true if all of the supplied objects have equal [[tape-primal]]s, false
  otherwise.

  Use [[equiv]] if you want to compare non-[[TapeCell]]s with
  [[TapeCell]]s and ignore all tangent components. If you _do_ want to take the
  tangent components into account, prefer [[eq]]."
  ([_] true)
  ([a b]
   (v/= (tape-primal a)
        (tape-primal b)))
  ([a b & more]
   (if (equiv a b)
     (if (next more)
       (recur b (first more) (next more))
       (equiv b (first more)))
     false)))

(defn compare
  "Comparator that compares [[TapeCell]] instances with each other or
  non-differentials using only the [[finite-part]] of each instance. Matches the
  response of [[equiv]].

  Acts as [[emmy.value/compare]] for non-[[TapeCell]]s."
  [a b]
  (v/compare
   (tape-primal a)
   (tape-primal b)))

;; ## Reverse-pass support

(defn ^:no-doc topological-sort
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
          (process-children [seen sorted in->partial]
            (transduce (map first)
                       (completing compute-visiting-order)
                       [seen sorted]
                       in->partial))]
    (second
     (compute-visiting-order [#{} []] node))))

;; [[reverse-phase]] will generate a map of node ID => partial derivative,
;; called a "sensitivity" while it's being accumulated. For functions that
;; return, say, a map of key => perturbed value, we need to run the reverse
;; phase for every value; rather than store a new map at each slot, we create a
;; new [[emmy.dual/Completed]] type to distinguish nested maps from sensitivity
;; maps.

(defn process [sensitivities tape]
  ;; Since we're processing in topological sort order, when we
  ;; reach a node we know we're done updating its
  ;; sensitivity (it can't be affected by nodes that come
  ;; after).
  (let [sensitivity (get sensitivities (tape-id tape))]
    (reduce
     (fn [sensitivities in-to-partial-entry]
       (let [[tape local-partial] in-to-partial-entry
             id                   (tape-id tape)
             delta                (g/* sensitivity local-partial)
             new-entry            (if-let [v (get sensitivities id)]
                                    (g/+ v delta)
                                    delta)]
         (assoc sensitivities id new-entry)))
     sensitivities
     (tape-partials tape))))

(defn ^:no-doc reverse-phase
  "Accepts a [[TapeCell]] `root` representing the final value, or output, of a
  reverse-mode derivative computation, and returns an [[emmy.dual/Completed]]
  instance wrapping a map of

  - each intermediate value seen in the computation to
  - the partial derivative of the output with respect to that value."
  [root]
  (let [nodes         (topological-sort root)
        sensitivities {(tape-id root) 1}]
    (d/->Completed
     (reduce process sensitivities nodes))))

;; [[reverse-phase]] above operates on a single [[TapeCell]]. For structured
;; outputs, we need to walk the output until we hit a [[TapeCell]] instance and
;; call [[reverse-phase]] for each. This will result in an output-shaped
;; structure of [[emmy.dual/Completed]] instances.
;;
;; Unfortunately we require two passes over the output structure. The first one
;; calls [[reverse-phase]] via [[emmy.dual/extract-tangent]] to generate the
;; maps of partials (stored in an [[emmy.dual/Completed]] instance), and the
;; second pass (via [[interpret]]) extracts the partial we care about.
;;
;; If we only have a single input, we could get away with a single pass and
;; combine these. But for multiple inputs, [[extract]] will be called multiple
;; times, one for each input.

(defn ^:no-doc interpret
  "Given

  - a perturbed input, either a [[TapeCell]] or structure of [[TapeCell]]s
  - an `output` [[emmy.dual/Completed]] instance or structure of completed instances
  - the `tag` for the current run of differentiation

  Returns a value with the same shape as `input`, but with each [[TapeCell]]
  leaf replaced by copies of `output` representing the partial derivative of the
  computation at that leaf."
  [input output tag]
  (cond (and (tape? input) (= tag (tape-tag input)))
        (d/extract-id output (tape-id input))

        (s/structure? input)
        (s/opposite input (mapv #(interpret % output tag) input))

        (f/function? input)
        (throw
         (ex-info "function inputs not supported." {:input input}))

        :else
        (throw
         (ex-info "unknown input type!" {:input input}))))

;; ## Gradient

(defn gradient
  "Given some differentiable function `f`, returns a function whose value at some
  point can multiply an increment in the arguments to produce the best linear
  estimate of the increment in the function value.

  For univariate functions, [[gradient]] computes a derivative. For
  vector-valued functions, [[gradient]] computes
  the [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
  of `f`.

  For numerical differentiation, see [[emmy.numerical.derivative/D-numeric]].

  NOTE: `f` must be built out of generic operations that know how to
  handle [[emmy.tape/TapeCell]] inputs in addition to any types that a
  normal `(f x)` call would present. This restriction does _not_ apply to
  operations like putting `x` into a container or destructuring; just primitive
  function calls."
  ([f] (gradient f []))
  ([f selectors]
   (fn grad
     ([] 0)
     ([x]
      (when (and (seq selectors) (not (s/structure? x)))
        (u/illegal
         (str "Selectors " selectors
              " not allowed for non-structural input " x)))
      (let [tag       (d/fresh-tag)
            input     (if-let [piece (get-in x selectors)]
                        (if (empty? selectors)
                          (tapify piece tag)
                          (assoc-in x selectors (tapify piece tag)))
                        ;; The call to `get-in` will return nil if the
                        ;; `selectors` don't index correctly into the supplied
                        ;; `input`, triggering this exception.
                        (u/illegal
                         (str "Bad selectors " selectors " for structure " x)))
            output    (d/with-active-tag tag f [input])
            completed (d/extract-tangent output tag d/REVERSE-MODE)]
        (-> (get-in input selectors)
            (interpret completed tag)))))))

(defmethod g/zero-like [::tape] [_] 0)
(defmethod g/one-like [::tape] [_] 1)
(defmethod g/identity-like [::tape] [_] 1)
(defmethod g/freeze [::tape] [t]
  `[~'TapeCell
    ~(tape-tag t)
    ~(tape-id t)
    ~(g/freeze (tape-primal t))
    ~(mapv (fn [[node partial]]
             [(g/freeze node) (g/freeze partial)])
           (tape-partials t))])

(defmethod g/simplify [::tape] [^TapeCell t]
  (TapeCell. (.-tag t)
             (.-id t)
             (g/simplify (.-primal t))
             (mapv (fn [[node partial]]
                     [(g/simplify node)
                      (g/simplify partial)])
                   (.-in->partial t))))
