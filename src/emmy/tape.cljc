#_"SPDX-License-Identifier: GPL-3.0";;

(ns emmy.tape
  "This namespace contains an implementation of [[TapeCell]], a type that forms
  the basis for the reverse-mode automatic differentiation implementation in
  Emmy.

  NOTE that this type currently can't handle any interaction with forward-mode
  AD! Don't nest [[gradient]] calls inside [[emmy.env/D]] calls."
  (:refer-clojure :exclude [compare zero?])
  (:require #?(:clj [clojure.pprint :as pprint])
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.operator :as o]
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
;; differentation, in contrast with the forward mode AD implementation TODO
;; below!. These two are closely related, and the implementations could merge
;; once I get around to reading and implementing [the YOLO
;; paper](https://arxiv.org/abs/2204.10923).
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
;;   - a "tag", used to prevent confusion between [[TapeCell]] instances created
;;     for different nested runs of differentation
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
;;  :id <fresh ID>
;;  :primal (f a b c ...)
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

;; ## IPerturbed Impl, stolen!

;; for some known `g` and `x`, but with the ability to store `(derivative
;; offset-fn)` and call it later with many different `g`.
;;
;; > NOTE: We might accomplish this by composing `extract-tangent` with the
;; > returned function, so that the extraction happens later, when the
;; > function's called... but that will fail. The real implementation is more
;; > subtle! See the [[emmy.calculus.derivative]] namespace for the actual
;; > implementation of [[IPerturbed]] for functions and multimethods.
;;
;; All of this suggests that we need to make [[extract-tangent]] an open
;; function that other folks can extend for other container-like
;; types ([functors](https://en.wikipedia.org/wiki/Functor), specifically).
;;
;; The [[IPerturbed]] protocol accomplishes this, along with two other functions
;; that we'll use later:

(defprotocol IPerturbed
  (perturbed? [this]
    "TODO clear this up, this is part of combining the interfaces for tape and
    differential. This is here so that literal functions can handle tape or
    differential instances..."
    #_"Returns true if the supplied object has some known non-zero tangent to be
    extracted via [[extract-tangent]], false otherwise. (Return `false` by
    default if you can't detect a perturbation.)")

  (replace-tag [this old-tag new-tag]
    "If `this` is perturbed, Returns a similar object with the perturbation
    modified by replacing any appearance of `old-tag` with `new-tag`. Else,
    return `this`.")

  (extract-tangent [this tag]
    "If `this` is perturbed, return the tangent component paired with the
    supplied tag. Else, returns `([[emmy.value/zero-like]] this)`."))

;; `replace-tag` exists to handle subtle bugs that can arise in the case of
;; functional return values. See the "Amazing Bug" sections
;; in [[emmy.calculus.derivative-test]] for detailed examples on how this
;; might bite you.
;;
;; The default implementations are straightforward, and match the docstrings:

(extend-protocol IPerturbed
  #?(:clj Object :cljs default)
  (perturbed? [_] false)
  (replace-tag [this _ _] this)
  (extract-tangent [this _] (g/zero-like this)))

;; ## TapeCell Implementation
;;
;; A [[TapeCell]] will respond to [[v/kind]] with `::tape`. To
;; allow [[TapeCell]] instances to work in any place that real numbers or
;; symbolic argument work, make `::tape` derive from `::v/scalar`:

(derive ::dual ::v/scalar)
(derive ::tape ::v/scalar)

;; TODO describe dual:

(declare compare compare-dual dual-primal)

(deftype Dual [tag primal tangent]
  v/IKind
  (kind [_] ::dual)

  IPerturbed
  ;; NOTE the reason we need this is for the arguments to literal function.
  ;; Those need to tell if there is some tape coming in. So we can probably
  ;; delete this now...
  (perturbed? [_] true)

  (replace-tag [this old new]
    ;; TODO think about why we don't have to recursively descend?? can
    ;; these show up more than one level deep? And... do I therefore not
    ;; need to do this for tapes either?
    (if (= old tag)
      (Dual. new primal tangent)
      this))

  (extract-tangent [_ t]
    (if (= t tag)
      tangent
      0))

  Object
  ;; TODO revisit all of this
  #?(:clj (equals [_ b] (v/= primal (dual-primal b))))
  #?(:cljs (valueOf [_] (.valueOf primal)))
  (toString [_]
    (str "#emmy.tape.Dual"
         {:tag tag
          :primal primal
          :tangent tangent}))

  #?@(:clj
      ;; The motivation for this override is subtle. To participate in control
      ;; flow operations, like comparison with both [[TapeCell]] and
      ;; non-[[TapeCell]] instances, [[TapeCell]] instances should compare using
      ;; ONLY their primal terms. This means that comparison will ignore any
      ;; difference in `in->partial`.
      [Comparable
       (compareTo [a b] (compare-dual a b))]

      :cljs
      [IComparable
       (-compare [a b]  (compare-dual a b))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer (.toString x)))]))

;; Here's the [[TapeCell]] type with the fields described above.

(deftype TapeCell [tag id primal in->partial]
  v/IKind
  (kind [_] ::tape)

  IPerturbed
  ;; NOTE the reason we need this is for the arguments to literal function.
  ;; Those need to tell if there is some tape coming in.
  (perturbed? [_] true)

  (replace-tag [this old new]
    ;; TODO think about why I don't have to do anything hard here...
    (if (= old tag)
      (TapeCell. new id primal in->partial)
      this))

  ;; This implementation is called if a tape ever makes it out of
  ;; forward-mode-differentiated function.
  (extract-tangent [_ _] 0)

  Object
  ;; Comparing [[TapeCell]] objects using `equals` defaults to [[equiv]], which
  ;; compares instances only using their non-tagged ('finite') components. If
  ;; you want to compare two instances using `tag` and `in->partial`,
  ;; See [[eq]].
  #?(:cljs (valueOf [_] (.valueOf primal)))
  (toString [_]
    (str "#emmy.tape.TapeCell"
         {:tag tag
          :id id
          :primal primal
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

(defn dual?
  "Returns true if the supplied object is an instance of [[Dual]], false
  otherwise."
  [x]
  (instance? Dual x))

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

(defn dual-tag
  [^Dual dual]
  (.-tag dual))

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

(defn dual-primal
  ([x]
   (if (dual? x)
     (.-primal ^Dual x)
     x))
  ([x tag]
   (if (and (dual? x) (= tag (dual-tag x)))
     (.-primal ^Dual x)
     x)))

(defn dual-tangent
  ([x]
   (if (dual? x)
     (.-tangent ^Dual x)
     0))
  ([x tag]
   (if (and (dual? x) (= tag (dual-tag x)))
     (.-tangent ^Dual x)
     0)))

(defn tape-id
  "Returns the `-id` field of the supplied [[TapeCell]] object. Errors if any
  other type is supplied.

  IDs are used in the reverse pass of automatic differentiation to prevent
  duplicating work for [[TapeCell]]s used as input to multiple
  other [[TapeCell]]s."
  [^TapeCell tape]
  (.-id tape))

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

;; More permissive accessors...

(defn tag-of
  "More permissive version of [[tape-tag]] that returns `nil` when passed a
  non-[[TapeCell]] instance."
  [x]
  (cond (tape? x)           (tape-tag x)
        (dual? x)           (dual-tag x)
        :else nil))

(defn primal-of
  "More permissive version of [[tape-primal]] that returns `v` when passed a
  non-[[TapeCell]]-or-[[Dual]] instance."
  ([v]
   (primal-of v (tag-of v)))
  ([v tag]
   (cond (tape? v)           (tape-primal v tag)
         (dual? v)           (dual-primal v tag)
         :else               v)))

(defn deep-primal
  "Version of [[tape-primal]] that will descend recursively into any [[TapeCell]]
  instance returned by [[tape-primal]] until encountering a non-[[TapeCell]].

  Given a non-[[TapeCell]], acts as identity."
  ([v]
   (cond (tape? v)           (recur (tape-primal v))
         (dual? v)           (recur (dual-primal v))
         :else               v)))

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
  ;; TODO bad docstring
  "Comparator that compares [[Tape]] instances with each other or
  non-differentials using only the [[finite-part]] of each instance. Matches the
  response of [[equiv]].

  Acts as [[emmy.value/compare]] for non-[[TapeCell]]s."
  [a b]
  (v/compare
   (tape-primal a)
   (tape-primal b)))

(defn compare-dual
  [a b]
  (v/compare
   (dual-primal a)
   (dual-primal b)))

;; ## Reverse-pass support

;; These first two functions create a way to globally declare, via a dynamic
;; binding, the stack of tags that are currently in play. If three nested
;; derivatives are being taken, [[*active-tags*]] will contain three entries
;; from a perspective inside the function at the deepest level.
;;
;; The [[IPerturbed]] implementation for functions uses this information to
;; determine whether or not to use [[replace-tag]] to protect its tag from
;; perturbation confusion. If some higher level is not trying to extract the
;; same tag, there's no need.

(def ^:dynamic *active-tags* [])

(let [next-tag (atom -1)]
  (defn fresh-tag
    "Returns a new, unique tag."
    []
    (swap! next-tag inc)))

(defn with-active-tag
  "Like `apply`, but conj-es `tag` onto the dynamic variable [[*active-tags*]]
  inside the scope of `f`.

  Returns the result of applying `f` to `args`."
  [tag f args]
  (binding [*active-tags* (conj *active-tags* tag)]
    (apply f args)))

(defn tag-active?
  "Returns true if `tag` is an element of [[*active-tags*]] (and therefore pending
  for extraction by some nested derivative), false otherwise."
  [tag]
  (boolean
   (some #{tag} (rseq *active-tags*))))

(defn inner-tag
  [& tags]
  (or (some (apply hash-set tags)
            (rseq *active-tags*))
      (apply max tags)))

(defn tag+perturbation
  "TODO we could change `perturbed?` into something like `possible-perturbations`,
  to get collection types to return sequence of inputs for this. Then we could
  handle map-shaped inputs etc into literal functions, if we had the proper
  descriptor language for it."
  ([& dxs]
   (let [m (into {} (mapcat
                     (fn [dx]
                       (when-let [t (tag-of dx)]
                         {t dx})))
                 dxs)]
     (when (seq m)
       (let [tag (apply inner-tag (keys m))]
         [tag (m tag)])))))

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
;; new [[Completed]] type to distinguish nested maps from sensitivity maps.
;;

(defrecord Completed [v->partial]
  IPerturbed
  (perturbed? [_] (boolean (some perturbed? (vals v->partial))))

  ;; TODO note that this can happen because these can pop out from inside of
  ;; ->partial-fn. And that is currently where the tag-rewriting has to occur.
  ;;
  ;; But that is going to be inefficient for lots of intermediate values...
  ;; ideally we could call this AFTER we select out the IDs. That implies that
  ;; we want to shove that inside of extract.
  ;;
  ;; TODO TODO TODO definitely do this, we definitely want that to happen, don't
  ;; have those stacked levels, otherwise super inefficient to walk multiple
  ;; times.
  ;;
  ;; TODO AND THEN if that's true then we can delete this implementation, since
  ;; we'll already be pulled OUT of the completed map.
  (replace-tag [_ old new]
    (Completed.
     (u/map-vals #(replace-tag % old new) v->partial)))

  ;; TODO note that this can never happen... because that means that
  ;; a [[Completed]] instance has been returned somehow from a gradient call.
  (extract-tangent [_ _]
    (assert "Impossible!")))

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
  reverse-mode derivative computation, and returns a [[Completed]] instance
  wrapping a map of

  - each intermediate value seen in the computation to
  - the partial derivative of the output with respect to that value."
  [root]
  (let [nodes         (topological-sort root)

        ;; TODO this is the spot where we want to wire in many sensitivities. So
        ;; how would it work, if we set all of the sensitivities for the outputs
        ;; at once? What would the ordering be as we walked backwards?
        sensitivities {(tape-id root) 1}]
    (->Completed
     (reduce process sensitivities nodes))))

;; [[reverse-phase]] above operates on a single [[TapeCell]]. For structured
;; outputs, we need to walk the output until we hit a [[TapeCell]] instance and
;; call [[reverse-phase]] for each. This will result in an output-shaped
;; structure of [[Completed]] instances.
;;
;; TODO [[->partials]] really should depend on `fmap`, as should so many other
;; things in the library. Without this we can't generically support output
;; values like maps or quaternions. [[extract-tangent]] does this for
;; forward-mode, I believe.

(declare ->partials)

;; TODO fix the docstring, and think of how we can combine this into the
;; narrative of what we find in derivative. Maybe this should be the main
;; version?

(defn- ->partials-fn
  "Returns a new function that composes a 'tag extraction' step with `f`. The
  returned fn will

  - call the underlying `f`, producing `result`
  - return `(->partials result tag)`

  If called within the scope of a function waiting for the same `tag`, the
  returned function will remap any instance of `tag` that appears in any
  differential argument passed to it to a private `fresh` tag, to prevent
  internal perturbation confusion. Any tangent components in the final result
  tagged with `fresh` will be remapped in the final result back to `tag`.

  If called _outside_ of a function waiting for `tag` no tag remapping will
  occur."
  [f tag]
  (-> (fn [& args]
        (if (tag-active? tag)
          (let [fresh (fresh-tag)]
            (-> (with-active-tag tag f (map #(replace-tag % tag fresh) args))
                (->partials tag)
                (replace-tag fresh tag)))
          (-> (with-active-tag tag f args)
              (->partials tag))))
      (f/with-arity (f/arity f))))

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

        ;; Here is an example of the subtlety. We MAY want to go one at a
        ;; time... or we may want to insert some sensitivity entry into the
        ;; entire structure and roll the entire structure back. We don't do that
        ;; YET so I bet we can get away with ignoring it for this first PR. But
        ;; we are close to needing that.
        (s/structure? output)
        (s/mapr #(->partials % tag) output)

        (f/function? output)
        (->partials-fn output tag)

        (o/operator? output)
        (o/->Operator (->partials (o/procedure output) tag)
                      (o/arity output)
                      (o/name output)
                      (o/context output)
                      (meta output))

        (v/scalar? output)
        (->Completed {})

        :else (u/unsupported "Output not handled yet!")))

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
        (comp #(extract % id) output)

        (o/operator? output)
        (o/->Operator (extract (o/procedure output) id)
                      (o/arity output)
                      (o/name output)
                      (o/context output)
                      (meta output))


        :else 0))

;; TODO note that [[interpret]] and [[tapify]] both need to become generic on
;; input-walking, and [[extract]] and [[->partials]] need to be generic on
;; output-walking.
;;
;; I am sure we are going to need to glom on to the [[replace-tag]] machinery as
;; well. [[extract-tangent]] is similar to what we want. Maybe we can share?

(defn ^:no-doc interpret
  "Given

  - a perturbed input, either a [[TapeCell]] or structure of [[TapeCell]]s
  - an `output` [[Completed]] instance or structure of completed instances
  - the `tag` for the current run of differentiation

  Returns a value with the same shape as `input`, but with each [[TapeCell]]
  leaf replaced by copies of `output` representing the partial derivative of the
  computation at that leaf."
  [input output tag]
  (cond
    (and (tape? input) (= tag (tape-tag input)))
    (extract output (tape-id input))

    (s/structure? input)
    (s/opposite input (mapv #(interpret % output tag) input))

    (f/function? input)
    (throw
     (ex-info "function inputs not supported." {:input input}))

    :else
    (throw
     (ex-info "unknown input type!" {:input input}))))

;; ## Gradient

;; starting to work on the one that returns a pair of primal and fn.

;; TODO try the version where we allow both...

#_
(defn gradient-fn
  ([f] (gradient-fn f []))
  ([f selectors]
   (fn
     ([] 0)
     ([x]
      (when (and (seq selectors) (not (s/structure? x)))
        (u/illegal
         (str "Selectors " selectors
              " not allowed for non-structural input " x)))

      (let [tag       (fresh-tag)
            inputs    (if (empty? selectors)
                        (tapify x tag)
                        (update-in x selectors tapify tag))
            output    (with-active-tag tag f [inputs])]
        [(tape-primal output)
         (if (empty? selectors)
           (fn []
             (let [completed (->partials output tag)]
               (interpret inputs completed tag)))
           (fn []
             (let [completed (->partials output tag)]
               (interpret (get-in inputs selectors) completed tag))))]))
     ([x & more]
      ((gradient-fn (fn [args]
                      (apply f args))
                    selectors)
       (matrix/seq-> (cons x more)))))))

;; ## Lifted Functions
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

(defn lift-1
  "Given:

  - some unary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument

  Returns a new unary function that operates on both the original type of
  `f`, [[TapeCell]] and [[Dual]] instances.

  If called without `df:dx`, `df:dx` defaults to `(f :dfdx)`; this will return
  the derivative registered to a generic function defined
  with [[emmy.util.def/defgeneric]].

  NOTE: `df:dx` has to ALREADY be able to handle [[TapeCell]] and [[Dual]]
  instances. The best way to accomplish this is by building `df:dx` out of
  already-lifted functions, and declaring them by forward reference if you need
  to."
  ([f]
   (if-let [df:dx (f :dfdx)]
     (lift-1 f df:dx)
     (u/illegal
      "No df:dx supplied for `f` or registered generically.")))
  ([f df:dx]
   (fn call [x]
     (cond (tape? x)
           (let [primal  (tape-primal x)
                 partial (df:dx primal)]
             (make (tape-tag x)
                   (call primal)
                   [[x partial]]))

           (dual? x)
           (let [px      (dual-primal x)
                 tx      (dual-tangent x)
                 partial (df:dx px)]
             (->Dual (dual-tag x)
                     (call px)
                     (if (g/numeric-zero? tx)
                       tx
                       (g/* partial tx))))

           :else (f x)))))

(defn lift-2
  "Given:

  - some binary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument
  - a function `df:dy`, similar to `df:dx` for the second arg

  Returns a new binary function that operates on both the original type of
  `f`, [[TapeCell]] and [[Dual]] instances.

  NOTE: `df:dx` and `df:dy` have to ALREADY be able to handle [[TapeCell]]
  and [[Dual]] instances. The best way to accomplish this is by building `df:dx`
  and `df:dy` out of already-lifted functions, and declaring them by forward
  reference if you need to."
  ([f]
   (let [df:dx (f :dfdx)
         df:dy (f :dfdy)]
     (if (and df:dx df:dy)
       (lift-2 f df:dx df:dy)
       (u/illegal
        "No df:dx, df:dy supplied for `f` or registered generically."))))
  ([f df:dx df:dy]
   (fn call [x y]
     (letfn [(operate-forward [tag]
               (let [xe (dual-primal x tag)
                     ye (dual-primal y tag)
                     dx (dual-tangent x tag)
                     dy (dual-tangent y tag)]
                 (->Dual tag
                         (call xe ye)
                         (g/+ (if (g/numeric-zero? dx)
                                dx
                                (g/* (df:dx xe ye) dx))
                              (if (g/numeric-zero? dy)
                                dy
                                (g/* (df:dy xe ye) dy))))))

             (operate-reverse [tag]
               (let [primal-x  (tape-primal x tag)
                     primal-y  (tape-primal y tag)
                     partial-x (if (and (tape? x) (= tag (tape-tag x)))
                                 [[x (df:dx primal-x primal-y)]]
                                 [])
                     partial-y (if (and (tape? y) (= tag (tape-tag y)))
                                 [[y (df:dy primal-x primal-y)]]
                                 [])]
                 (make tag
                       (call primal-x primal-y)
                       (into partial-x partial-y))))]
       (if-let [[tag dx] (tag+perturbation x y)]
         (cond (tape? dx)           (operate-reverse tag)
               (dual? dx)           (operate-forward tag)
               :else
               (u/illegal "Non-tape or dual perturbation!"))
         (f x y))))))

(defn lift-n
  "Given:

  - some function `f` that can handle 0, 1 or 2 arguments
  - `df:dx`, a fn that returns the derivative wrt the single arg in the unary case
  - `df:dx1` and `df:dx2`, fns that return the derivative with respect to the
    first and second args in the binary case

  Returns a new any-arity function that operates on both the original type of
  `f` and [[TapeCell]] instances.

  NOTE: The n-ary case of `f` is populated by nested calls to the binary case.
  That means that this is NOT an appropriate lifting method for an n-ary
  function that isn't built out of associative binary calls. If you need this
  ability, please file an issue at the [emmy issue
  tracker](https://github.com/mentat-collective/emmy/issues)."
  [f df:dx df:dx1 df:dx2]
  (let [f1 (lift-1 f df:dx)
        f2 (lift-2 f df:dx1 df:dx2)]
    (fn call
      ([] (f))
      ([x] (f1 x))
      ([x y] (f2 x y))
      ([x y & more]
       (reduce call (call x y) more)))))

;; ## Generic Method Installation
;;
;; Armed with [[lift-1]] and [[lift-2]], we can install [[TapeCell]]
;; and [[Dual]] into the Emmy generic arithmetic system.
;;
;; Any function built out of these components will work with
;; the [[emmy.calculus.derivative/D]] operator.

(defn- defunary
  "Given:

  - a generic unary multimethod `generic-op`
  - optionally, a corresponding single-arity lifted function
    `differential-op` (defaults to `(lift-1 generic-op)`)

  installs an appropriate unary implementation of `generic-op` for
  perturbations."
  ([generic-op]
   (defunary generic-op (lift-1 generic-op)))
  ([generic-op differential-op]
   (defmethod generic-op [::tape] [a] (differential-op a))
   (defmethod generic-op [::dual] [a] (differential-op a))))

(defn- defbinary
  "Given:

  - a generic binary multimethod `generic-op`
  - optionally, a corresponding 2-arity lifted function
    `differential-op` (defaults to `(lift-2 generic-op)`)

  installs an appropriate binary implementation of `generic-op` between
  perturbations and `::v/scalar` instances."
  ([generic-op]
   (defbinary generic-op (lift-2 generic-op)))
  ([generic-op differential-op]
   (doseq [signature [[::tape ::tape]
                      [::dual ::dual]
                      [::tape ::dual]
                      [::dual ::tape]
                      [::tape ::v/scalar]
                      [::v/scalar ::tape]
                      [::dual ::v/scalar]
                      [::v/scalar ::dual]]]
     (defmethod generic-op signature [a b] (differential-op a b)))))

(defn ^:no-doc by-primal
  "Given some unary or binary function `f`, returns an augmented `f` that acts on
  the primal entries of any [[TapeCell]] arguments encounted, irrespective of
  tag.

  Given a perturbation with a perturbation in its [[primal-part]], the returned
  `f` will recursively descend until it hits a non-perturbation."
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
    (let [f    (deep-primal x)
          func (cond (< f 0) (lift-1 g/negate (fn [_] -1))
                     (> f 0) (lift-1 identity (fn [_] 1))
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
(defunary g/zero? (by-primal g/zero?))
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

;; DUAL:

(defmethod g/zero-like [::dual] [_] 0)
(defmethod g/one-like [::dual] [_] 1)
(defmethod g/identity-like [::dual] [_] 1)
(defmethod g/freeze [::dual] [t]
  `[~'Dual
    ~(dual-tag t)
    ~(g/freeze (dual-primal t))
    ~(g/freeze (dual-tangent t))])

(defmethod g/simplify [::dual] [^Dual t]
  (Dual. (.-tag t)
         (g/simplify (.-primal t))
         (g/simplify (.-tangent t))))

;; TODO merge into the above:

;; ## Differentials, Dual Numbers and Automatic Differentiation
;;
;; This namespace develops an implementation of a type called [[Differential]].
;; A [[Differential]] is a generalization of a type called a ["dual
;; number"](https://en.wikipedia.org/wiki/Dual_number).
;;
;; As we'll discuss, passing these numbers as arguments to some function $f$
;; built out of the [[emmy.generic]] operators allows us to build up the
;; _derivative_ of $f$ in parallel to our evaluation of $f$. Complex programs
;; are built out of simple pieces that we know how to evaluate; we can build up
;; derivatives of entire programs in a similar way by building them out of the
;; derivatives of the smaller pieces of those programs.
;;

;; ### Forward-Mode Automatic Differentiation
;;
;; For many scientific computing applications, it's valuable be able to generate
;; a "derivative" of a function; given some tiny increment in the inputs, what
;; tiny increment will the function produce in the output values?
;;
;; we know how to take derivatives of many of the generic functions exposed by
;; Emmy, like [[+]], [[*]], [[g/sin]] and friends. It turns out that we can
;; take the derivatives of large, complicated functions by combining the
;; derivatives of these smaller functions using the [chain
;; rule]((https://en.wikipedia.org/wiki/Automatic_differentiation#The_chain_rule,_forward_and_reverse_accumulation))
;; as a clever bookkeeping device.
;;
;; The technique of evaluating a function and its derivative in parallel is
;; called "forward-mode [Automatic
;; Differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation)".
;; The [Emmy
;; wiki](https://github.com/mentat-collective/emmy/wiki/Automatic-Differentiation)
;; has more information on the history of this technique, and links to the many
;; other implementations you'll find in different languages. See the [cljdocs
;; Automatic Differentiation
;; page](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/calculus/automatic-differentiation)
;; for "how do I use this?"-style questions.
;;
;; > NOTE: The other flavor of automatic differentiation (AD) is "reverse-mode
;; > AD". See [[emmy.tape]] for an implementation of this style, coming soon!
;;
;; ### Dual Numbers and AD
;;
;; Our goal is to build up derivatives of complex functions out of the
;; derivatives of small pieces. A [dual
;; number](https://en.wikipedia.org/wiki/Dual_number) is a relatively simple
;; piece of machinery that will help us accomplish this goal.

;; A [dual number](https://en.wikipedia.org/wiki/Dual_number) is a pair of
;; numbers of the form
;;
;; $$a + b \varepsilon$$
;;
;; where $a$ and $b$ are real numbers, and $\varepsilon$ is an abstract thing,
;; with the property that $\varepsilon^2 = 0$.

;; > NOTE: This might remind you of the definition of a complex number of the
;; > form $a + bi$, where $i$ is also a new thing with the property that $i^2 =
;; > -1$. You are very wise! The bigger idea lurking here is the ["generalized
;; > complex
;; > number"](https://people.rit.edu/harkin/research/articles/generalized_complex_numbers.pdf).
;;
;; Why are dual numbers useful (in Emmy)? If you pass $a+b\varepsilon$ in
;; to a function $f$, the result is a dual number $f(a) + Df(a) b \varepsilon$;
;; the result contains both the function evaluation and the derivative
;; evaluation at $a$!

;; To see why, look at what happens when you pass a dual number into the [Taylor
;; series expansion](https://en.wikipedia.org/wiki/Taylor_series) of some
;; arbitrary function $f$. As a reminder, the Taylor series expansion of $f$
;; around some point $a$ is:
;;
;; $$f(x) = f(a)+\frac{Df(a)}{1!}(x-a)+\frac{D^2f(a)}{2!}(x-a)^{2}+\frac{D^3f(a)}{3!}(x-a)^{3}+\cdots$$
;;
;; > NOTE: See this nice overview of [Taylor series
;; > expansion](https://medium.com/@andrew.chamberlain/an-easy-way-to-remember-the-taylor-series-expansion-a7c3f9101063)
;; > by Andrew Chamberlain if you want to understand this idea and why we can
;; > approximate (smooth) functions this way.
;;
;; If you evaluate the expansion of $f(x)$ around $a$ with a dual number
;; argument whose first component is $a$ -- take $x=a+b\varepsilon$, for example
;; -- watch how the expansion simplifies:
;;
;; $$f(a+b\varepsilon) = f(a)+\frac{Df(a)}{1!}(b\varepsilon)+\frac{D^2f(a)}{2!}(b\varepsilon)^2+\cdots$$
;;
;; Since $\varepsilon^2=0$ we can ignore all terms beyond the first two:
;;
;; $$f(a+b\varepsilon) = f(a)+ (Df(a)b)\varepsilon$$
;;
;; > NOTE: See [[lift-1]] for an implementation of this idea.
;;
;; This justifies our claim above: applying a function to some dual number
;; $a+\varepsilon$ returns a new dual number, where
;;
;; - the first component is $f(a)$, the normal function evaluation
;; - the second component is $Df(a)$, the derivative.
;;
;; If we do this twice, the second component of the returned dual number
;; beautifully recreates the [Chain
;; Rule](https://en.wikipedia.org/wiki/Chain_rule):
;;
;; $$
;; \begin{aligned}
;; g(f(a+\varepsilon)) &= g(f(a) + Df(a)\varepsilon) \\
;; &= g(f(a)) + (Dg(f(a)))(Df(a))\varepsilon
;; \end{aligned}
;; $$
;;
;; ### Terminology Change
;;
;; A "dual number" is a very general idea. Because we're interested in dual
;; numbers as a bookkeeping device for derivatives, we're going to specialize
;; our terminology. From now on, we'll rename $a$ and $b$ to $x$ and $x'$. Given
;; a dual number of the form $x+x'\varepsilon$: we'll refer to:
;;
;; - $x$ as the "primal" part of the dual number
;; - $x'$ as the "tangent" part
;; - $\varepsilon$ as the "tag"
;;
;; > NOTE: "primal" means $x$ is tracking the "primal", or "primary", part of
;; > the computation. "tangent" is a synonym for "derivative". "tag" is going to
;; > make more sense shortly, when we start talking about mixing together
;; > multiple $\varepsilon_1$, $\varepsilon_2$ from different computations.
;;
;; ### Binary Functions
;;
;; What about functions of more than one variable? We can use the same approach
;; by leaning on the [multivariable Taylor series
;; expansion](https://en.wikipedia.org/wiki/Taylor_series#Taylor_series_in_several_variables).
;; Take $f(x, y)$ as a binary example. If we pass dual numbers in to the taylor
;; series expansion of $f$, the $\varepsilon$ multiplication rule will erase all
;; higher-order terms, leaving us with:
;;
;; $$f(x+x'\varepsilon, y+y'\varepsilon) = f(x,y) + \left[\partial_1 f(x,y)x' + \partial_2 f(x,y)y'\right]\varepsilon$$
;;
;; > NOTE: See [[lift-2]] for an implementation of this idea.
;;
;; This expansion generalizes for n-ary functions; every new argument $x_n +
;; x'_n\varepsilon$ contributes $\partial_n f(...)x'_n$ to the result.
;;
;; We can check this with the simple cases of addition, subtraction and
;; multiplication.
;;
;; The real parts of a dual number add commutatively, so we can rearrange the
;; components of a sum to get a new dual number:
;;
;; $$(x+x'\varepsilon)+(y+y'\varepsilon) == (x+y)+(x'+y')\varepsilon$$
;;
;; This matches the [sum
;; rule](https://en.wikipedia.org/wiki/Differentiation_rules#Differentiation_is_linear)
;; of differentiation, since the partials of $x + y$ with respect to either $x$
;; or $y$ both equal 1.
;;
;; Subtraction is almost identical and agrees with the [subtraction
;; rule](https://en.wikipedia.org/wiki/Differentiation_rules#Differentiation_is_linear):
;;
;; $$(x+x'\varepsilon)-(y+y'\varepsilon) == (x-y)+(x'-y')\varepsilon$$
;;
;; Multiplying out the components of two dual numbers again gives us a new dual
;; number, whose tangent component agrees with the [product
;; rule](https://en.wikipedia.org/wiki/Product_rule):
;;
;; $$
;; \begin{aligned}
;; (x+ x'\varepsilon)*(y+y'\epsilon) &= xy+(xy')\varepsilon+(x'y)\varepsilon+(x'y')\epsilon^2 \\
;; &= xy+(xy'+x'y)\varepsilon
;; \end{aligned}
;; $$
;;
;; Stare at these smaller derivations and convince yourself that they agree with
;; the Taylor series expansion method for binary functions.
;;
;; The upshot is that, armed with these techniques, we can implement a
;; higher-order `derivative` function (almost!) as simply as this:

(comment
  (defn derivative [f]
    (fn [x]
      (extract-tangent
       (f (make-dual x 1))))))

;; As long as `f` is built out of functions that know how to apply themselves to
;; dual numbers, this will all Just Work.
;;
;; ### Multiple Variables, Nesting
;;
;; All of the examples above are about first-order derivatives. Taking
;; higher-order derivatives is, in theory, straightforward:

(comment
  (derivative
   (derivative f)))

;; But this guess hits one of many subtle problems with the implementation of
;; forward-mode AD. The double-call to `derivative` will expand out to this:

(comment
  (fn [x]
    (letfn [(inner-d [x]
              (extract-tangent
               (f (make-dual x 1))))]
      (extract-tangent
       (inner-d
        (make-dual x 1))))))

;; the `x` received by `inner-d` will ALREADY be a dual number $x+\varepsilon$!
;; This will cause two immediate problems:
;;
;; - `(make-dual x 1)` will return $(x+\varepsilon)+\varepsilon = x+2\varepsilon$,
;;    which is not what we we want

;; - The `extract-tangent` call inside `inner-d` will return the `Df(x)`
;;   component of the dual number... which, remember, is no longer a dual
;;   number! So the SECOND call to `extract-tangent` have nothing to extract,
;;   and can only sensibly return 0.
;;
;; The problem here is called "perturbation confusion", and is covered in great
;; detail in
;; ["Confusion of Tagged Perturbations in Forward Automatic Differentiation of
;; Higher-Order Functions"](https://arxiv.org/abs/1211.4892), by Manzyuk et
;; al. (2019).
;;
;; The solution is to introduce a new $\varepsilon$ for every level, and allow
;; different $\varepsilon$ instances to multiply without annihilating. Each
;; $\varepsilon$ is called a "tag". [[Differential]] (implemented below) is a
;; generalized dual number that can track many tags at once, allowing nested
;; derivatives like the one described above to work.
;;
;; This implies that `extract-tangent` needs to take a tag, to determine _which_
;; tangent to extract:

(comment
  (defn derivative [f]
    (let [tag (fresh-tag)]
      (fn [x]
        (-> (f (make-dual x 1 tag))
            (extract-tangent tag))))))

;; This is close to the final form you'll find
;; at [[emmy.calculus.derivative/derivative]].
;;
;; ### What Return Values are Allowed?
;;
;; Before we discuss the implementation of dual
;; numbers (called [[Differential]]), [[lift-1]], [[lift-2]] and the rest of the
;; machinery that makes this all possible; what sorts of objects is `f` allowed
;; to return?
;;
;; The dual number approach is beautiful because we can bring to bear all sorts
;; of operations in Clojure that never even _see_ dual numbers. For example,
;; `square-and-cube` called with a dual number returns a PAIR of dual numbers:

(comment
  (defn square-and-cube [x]
    (let [x2 (g/square x)
          x3 (g/cube x)]
      [x2 x3])))

;; Vectors don't care what they contain! We want the derivative of
;; `square-and-cube` to also return a vector, whose entries represent the
;; derivative of _that entry_ with respect to the function's input.
;;
;; But this implies that [[extract-tangent]] from the example above needs to
;; know how to handle vectors and other collections; in the case of a vector `v`
;; by returning `(mapv extract-tangent v)`.
;;
;; What about higher-order functions?

(comment
  (defn offset-fn
    "Returns a function that takes a single-argument function `g`, and returns a new
  function like `g` that offsets its input by `offset`."
    [offset]
    (fn [g]
      (fn [x]
        (g (+ x offset))))))

;; `(derivative offset-fn)` here returns a function! Manzyuk et al. 2019 makes
;; the reasonable claim that, if `(f x)` returns a function, then `(derivative
;; f)` should treat `f` as a multi-argument function with its first argument
;; curried.
;;
;; Let's say `f` takes a number `x` and returns a function `g` that maps number
;; => number. `(((derivative f) x) y)` should act just like the partial
;; derivative of the equivalent multi-argument function, with respect to the
;; first argument:
;;
;;```clj
;;(((partial 0) f-flattened) x y)
;;```
;;
;; In other words, `(derivative offset-fn)` should act just like:

(comment
  (derivative
   (fn [offset] (g (+ x offset)))))
