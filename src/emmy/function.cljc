#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.function
  "Procedures that act on Clojure's function and multimethod types, along with
  extensions of the Emmy generic operations to functions.

  See [the `Function`
  cljdocs](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/data-types/function)
  for a discussion of generic function arithmetic."
  (:refer-clojure :exclude [get get-in memoize with-meta name])
  (:require [clojure.core :as core]
            [clojure.core.match :refer [match]]
            [emmy.dual :as d]
            [emmy.generic :as g]
            [emmy.util :as u]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang AFunction Fn RestFn MultiFn Keyword Symbol Var)
              (java.lang.reflect Method))))

;; ## Function Algebra
;;
;; this namespace extends the emmy generic operations to Clojure functions
;; and multimethods. (Of course, this includes the generic operations
;; themselves!)

;; ### Utilities

(defprotocol IArity
  (arity [f]
    "Return the cached or obvious arity of `f` if we know it. Otherwise
    delegates to heavy duty reflection."))

(extend-protocol IArity
  #?(:clj Object :cljs default)
  (arity [o]
    (or (:arity (meta o))
        ;; Faute de mieux, we assume the function is unary. Most math functions
        ;; are.
        [:exactly 1]))

  Symbol
  (arity [_] [:exactly 0])

  MultiFn
  ;; If f is a multifunction, then we expect that it has a multimethod
  ;; responding to the argument :arity, which returns the arity.
  (arity [f] (f :arity)))

(defn function?
  "Returns true if `f` is of [[v/kind]] `::v/function`, false otherwise."
  [f]
  (isa? (v/kind f) ::v/function))

(defn- with-meta
  "The current Clojurescript definition of `with-meta` first tests its
   argument with `js-fn?` and generates a MetaFn if so, frustrating our
   definition of IWithMeta on native JS function objects. This wrapper
   delegates to our definition, which allows native functions to safely
   carry metadata. Note that in Clojure one is guaranteed a fresh object
   with the new metadata, but in Clojurescript the target is mutated.
   This function is safe to use on freshly created functions, but may
   require careful consideration in other contexts."
  [f m]
  #?(:cljs (v/set-js-meta! f m)
     :clj (clojure.core/with-meta f m)))

(defn with-arity
  "Appends the supplied `arity` to the metadata of `f`, knocking out any
  pre-existing arity notation.

  Optionally accepts a third parameter `m` of metadata to attach to the return
  function, in addition to the new `:arity` key."
  ([f arity]
   (with-arity f arity {}))
  ([f arity m]
   (let [new-meta (-> (meta f)
                      (merge m)
                      (assoc :arity arity))]
     (with-meta f new-meta))))

(defn compose
  "Arity-preserving version of `clojure.core/comp`.

  The arity of a composition is the arity of the rightmost (that is, first to be
  applied) function term in `fns`."
  [& fns]
  (let [a (arity (or (last fns)
                     identity))]
    (with-arity (apply comp fns) a)))

(defn memoize
  "meta-preserving version of `clojure.core/memoize`.

  The returned function will have a new `:arity` entry in its metadata with the
  `arity` of the original `f`; this is because the process used to figure out a
  function's arity will not work across the memoization boundary."
  [f]
  (let [m (meta f)
        m (if (:arity m)
            m
            (assoc m :arity (arity f)))]
    (with-meta (core/memoize f) m)))

(defn get
  "For non-functions, acts like [[clojure.core/get]]. For function
  arguments (anything that responds true to [[function?]]), returns

  ```clojure
  (comp #(clojure.core/get % k) f)
  ```

  If `not-found` is supplied it's passed through to the
  composed [[clojure.core/get]]."
  ([f k]
   (if (function? f)
     (compose #(get % k) f)
     (core/get f k)))
  ([f k not-found]
   (if (function? f)
     (compose #(get % k not-found) f)
     (core/get f k not-found))))

(defn get-in
  "For non-functions, acts like [[clojure.core/get-in]]. For function
  arguments (anything that responds true to [[function?]]), returns

  ```clojure
  (comp #(clojure.core/get-in % ks) f)
  ```

  If `not-found` is supplied it's passed through to the
  composed [[clojure.core/get-in]]."
  ([f ks]
   (if (function? f)
     (compose #(get-in % ks) f)
     (core/get-in f ks)))
  ([f ks not-found]
   (if (function? f)
     (compose #(get-in % ks not-found) f)
     (core/get-in f ks not-found))))

(defn- zero-like [f]
  (-> (fn [& args]
        (g/zero-like (apply f args)))
      (with-arity (arity f) {:from :zero-like})))

(defn- one-like [f]
  (-> (fn [& args]
        (g/one-like (apply f args)))
      (with-arity (arity f) {:from :one-like})))

(def I
  "Identity function. Returns its argument."
  identity)

(defn- identity-like [f]
  (with-arity identity (arity f) {:from :identity-like}))

(defn arg-shift
  "Takes a function `f` and a sequence of `shifts`, and returns a new function
  that adds each shift to the corresponding argument of `f`. Too many or two few
  shifts are ignored.

  ```clojure
  ((arg-shift square 3) 4) ==> 49
  ((arg-shift square 3 2 1) 4) ==> 49
  ```"
  [f & shifts]
  (let [shifts (concat shifts (repeat 0))]
    (-> (fn [& xs]
          (apply f (map g/+ xs shifts)))
        (with-arity (arity f)))))

(defn arg-scale
  "Takes a function `f` and a sequence of `factors`, and returns a new function
  that multiplies each factor by the corresponding argument of `f`. Too many or
  two few factors are ignored.

  ```clojure
  ((arg-scale square 3) 4) ==> 144
  ((arg-scale square 3 2 1) 4) ==> 144
  ```"
  [f & factors]
  (let [factors (concat factors (repeat 1))]
    (-> (fn [& xs]
          (apply f (map g/* xs factors)))
        (with-arity (arity f)))))

(extend-protocol v/IKind
  MultiFn
  (kind [_] ::v/function)

  #?(:clj AFunction :cljs function)
  (kind [_] ::v/function)

  Var
  (kind [_] ::v/function)

  #?@(:cljs [MetaFn
             (kind [_] ::v/function)]))

;; we record arities as a vector with an initial keyword:
;;   [:exactly m]
;;   [:between m n]
;;   [:at-least m]

#?(:clj
   (do (defn ^:no-doc arity-map [f]
         (let [^"[Ljava.lang.reflect.Method;" methods (.getDeclaredMethods (class f))
               ;; tally up arities of invoke, doInvoke, and getRequiredArity
               ;; methods. Filter out invokeStatic.
               pairs (for [^Method m methods
                           :let [name (.getName m)]
                           :when (not (#{"withMeta" "meta" "invokeStatic"} name))]
                       (condp = name
                         "invoke"   [:invoke (alength (.getParameterTypes m))]
                         "doInvoke" [:doInvoke true]
                         "getRequiredArity" [:getRequiredArity
                                             (.getRequiredArity ^RestFn f)]))
               facts (group-by first pairs)]
           {:arities        (into #{} (map peek) (:invoke facts))
            :required-arity (second (first (:getRequiredArity facts)))
            :invoke?        (boolean (seq (:doInvoke facts)))}))

       (defn ^:no-doc jvm-arity [f]
         (let [{:keys [arities required-arity invoke?] :as m} (arity-map f)]
           (cond
             ;; Rule one: if all we have is one single case of invoke, then the
             ;; arity is the arity of that method. This is the common case.
             (and (= 1 (count arities))
                  (not required-arity)
                  (not invoke?))
             [:exactly (first arities)]

             ;; Rule two: if we have invokes for the arities 0..3,
             ;; getRequiredArity says 3, and we have doInvoke, then we consider that
             ;; this function was probably produced by Clojure's core "comp"
             ;; function, and we somewhat lamely consider the arity of the composed
             ;; function 1.
             (and (= #{0 1 2 3} arities)
                  (= 3 required-arity)
                  invoke?)
             [:exactly 1]

             ;; Rule three: if we have exactly one doInvoke and getRequiredArity,
             ;; then the arity at least the result of .getRequiredArity.
             (and required-arity
                  invoke?)
             [:at-least (apply min required-arity arities)]

             ;; Rule four: If we have more than 1 `invoke` clause, return a
             ;; `:between`. This won't account for gaps between the arities.
             (seq arities)
             [:between
              (apply min arities)
              (apply max arities)]

             :else
             (u/illegal
              (str "Not enough info to determine jvm-arity of " f " :" m))))))

   :cljs
   (do
     (defn ^:no-doc variadic?
       "Returns true if the supplied function is variadic, false otherwise."
       [f]
       (boolean
        (.-cljs$core$IFn$_invoke$arity$variadic f)))

     (defn ^:no-doc exposed-arities
       "When CLJS functions have different arities, the function is represented as a js
  object with each arity storied under its own key."
       [f]
       (let [pattern (re-pattern #"invoke\$arity\$\d+")
             parse   (fn [s]
                       (when-let [arity (re-find pattern s)]
                         (js/parseInt (subs arity 13))))
             arities (->> (map parse (js-keys f))
                          (concat [(.-cljs$lang$maxFixedArity f)])
                          (remove nil?)
                          (into #{}))]
         (if (empty? arities)
           [(alength f)]
           (sort arities))))

     (defn ^:no-doc js-arity
       "Returns a data structure indicating the arity of the supplied function."
       [f]
       (let [arities (exposed-arities f)]
         (cond (variadic? f)
               (if (= [0 1 2 3] arities)
                 ;; Rule 3, where we assume that any function that's variadic and
                 ;; that has defined these particular arities is a "compose"
                 ;; function... and therefore takes a single argument.
                 [:exactly 1]

                 ;; this case is where we know we have variadic args, so we set
                 ;; a minimum. This could break if some arity was missing
                 ;; between the smallest and the variadic case.
                 [:at-least (first arities)])

               ;; This corresponds to rule 1 in the JVM case. We have a single
               ;; arity and no evidence of a variadic function.
               (= 1 (count arities)) [:exactly (first arities)]

               ;; This is a departure from the JVM rules. A potential error here
               ;; would occur if someone defined arities 1 and 3, but missed 2.
               :else [:between
                      (first arities)
                      (last arities)])))))

(def ^:no-doc reflect-on-arity
  "Returns the arity of the function f. Computing arities of clojure
  functions is a bit complicated. It involves reflection, so the results are
  definitely worth memoizing."
  (core/memoize
   #?(:cljs js-arity :clj jvm-arity)))

(def ^:dynamic *strict-arity-checks*
  "If true, attempting to pass two functions of incompatible arity
  into any binary function, or into [[combine-arities]], will throw. False by
  default."
  false)

#?(:clj
   (extend-protocol IArity
     AFunction
     (arity [f] (:arity (meta f) (reflect-on-arity f))))

   :cljs
   (extend-protocol IArity
     function
     (arity [f] (:arity (meta f) (reflect-on-arity f)))

     MetaFn
     (arity [f] (:arity (meta f) (reflect-on-arity f)))))

(defn combine-arities
  "Returns the joint arity of arities `a` and `b`.

  The joint arity is the loosest possible arity specification compatible with
  both `a` and `b`. Throws if `a` and `b` are incompatible."
  ([] [:at-least 0])
  ([a] a)
  ([a b]
   (letfn [(fail []
             (if *strict-arity-checks*
               (u/illegal (str "Incompatible arities: " a " " b))
               [:at-least 0]))]
     ;; since the combination operation is symmetric, sort the arguments
     ;; so that we only have to implement the upper triangle of the
     ;; relation.
     (if (pos? (compare (first a) (first b)))
       (combine-arities b a)
       (match [a b]
         [[:at-least k] [:at-least k2]] [:at-least (max k k2)]
         [[:at-least k] [:between m n]] (let [m (max k m)]
                                          (cond (= m n) [:exactly m]
                                                (< m n) [:between m n]
                                                :else (fail)))
         [[:at-least k] [:exactly l]] (if (>= l k)
                                        [:exactly l]
                                        (fail))
         [[:between m n] [:between m2 n2]] (let [m (max m m2)
                                                 n (min n n2)]
                                             (cond (= m n) [:exactly m]
                                                   (< m n) [:between m n]
                                                   :else (fail)))
         [[:between m n] [:exactly k]] (if (<= m k n)
                                         [:exactly k]
                                         (fail))
         [[:exactly k] [:exactly l]] (if (= k l) [:exactly k] (fail)))))))

(defn joint-arity
  "Find the most relaxed possible statement of the joint arity of the given sequence of `arities`.
  If they are incompatible, an exception is thrown."
  [arities]
  (reduce combine-arities arities))

(defn seq-arity
  "Returns the most general arity compatible with the aritiies of all entries in
  the supplied sequence `xs` of values."
  [xs]
  (transduce (map arity) combine-arities xs))

;; ## Generic Implementations
;;
;; A `::cofunction` is a type that we know how to combine with a function in a
;; binary operation.

(derive ::v/scalar ::cofunction)

(defn- unary-operation
  "For a unary function `f` (like [[g/sqrt]]), returns a function of one function
  `g`. The returned function acts like `(comp f g)`. For example:

  ```clojure
  (([[unary-operation]] f) g)
  ;;=> (fn [x] (f (g x)))
  ```"
  [f]
  (-> (partial comp f)
      (with-arity [:exactly 1])))

(defn coerce-to-fn
  "Given an [[emmy.value/scalar?]] input `x`, returns a function of arity `arity`
  that always returns `x` no matter what input it receives.

  For non-numerical `x`, returns `x`."
  ([x arity]
   (if (v/scalar? x)
     (-> (constantly x)
         (with-arity arity))
     x)))

(defn- binary-operation
  "Accepts a binary function `op`, and returns a function of two functions `f` and
  `g` which will produce the pointwise operation `op` of the results of applying
  both `f` and `g` to the input.

  For example:

  ```clojure
  (([[binary-operation]] op) f g)
  ;;=> (fn [x] (op (f x) (g x)))
  ```"
  [op]
  (letfn [(h [f g]
            (let [f-arity (if (v/scalar? f) (arity g) (arity f))
                  g-arity (if (v/scalar? g) f-arity   (arity g))
                  f1      (coerce-to-fn f f-arity)
                  g1      (coerce-to-fn g g-arity)
                  arity   (joint-arity [f-arity g-arity])]
              (with-arity (fn [& args] (op (apply f1 args) (apply g1 args))) arity)))]
    (with-arity h [:exactly 2])))

(defn- defunary
  "Given a generic unary function `generic-op`, define the multimethods necessary
  to introduce this operation to function arguments."
  [generic-op]
  (let [unary-op (unary-operation generic-op)]
    (defmethod generic-op [::v/function] [a]
      (unary-op a))))

(defn- defbinary
  "Given a generic binary function `generic-op` (and an optional `binary-op` to
  perform the work), define the multimethods necessary to introduce this
  operation to function arguments."
  ([generic-op] (defbinary generic-op generic-op))
  ([generic-op binary-op]
   (let [binop (binary-operation binary-op)]
     (doseq [signature [[::v/function ::v/function]
                        [::v/function ::cofunction]
                        [::cofunction ::v/function]]]
       (defmethod generic-op signature [a b]
         (binop a b))))))

(defbinary g/add g/+)
(defbinary g/sub g/-)
(defbinary g/mul g/*)
(defunary g/invert)
(defbinary g/div g/divide)
(defbinary g/expt)
(defunary g/sqrt)

(defunary g/negate)
(defunary g/negative?)
(defunary g/abs)
(defunary g/floor)
(defunary g/ceiling)
(defunary g/integer-part)
(defunary g/fractional-part)

(defbinary g/quotient)
(defbinary g/remainder)
(defbinary g/modulo)

(defunary g/sin)
(defunary g/cos)
(defunary g/tan)
(defunary g/asin)
(defunary g/acos)

(defunary g/atan)
(defbinary g/atan)

(defunary g/sinh)
(defunary g/cosh)
(defunary g/tanh)

(defunary g/square)
(defunary g/cube)

(defunary g/exp)
(defunary g/log)

(comment
  "This comment expands on a comment from scmutils, function.scm, in the
  definition of `transpose-defining-relation`:

  $T$ is a linear transformation

  $$T : V -> W$$

  the transpose of $T$ is

  $$T^t : (W -> R) -> (V -> R)$$

  \\forall a \\in V, g \\in (W -> R),

  T^t : g \\to g \\circ T

  ie:

  (T^t(g))(a) = g(T(a))")
(defmethod g/transpose [::v/function] [f]
  (fn [g]
    (fn [a]
      (g (f a)))))

(defunary g/determinant)
(defunary g/trace)

(defbinary g/gcd)
(defbinary g/lcm)
(defbinary g/exact-divide)

(defbinary g/solve-linear)
(defbinary g/solve-linear-right)

(defunary g/dimension)
(defbinary g/dot-product)
(defbinary g/inner-product)
(defbinary g/outer-product)
(defbinary g/cross-product)

;; Complex Operations

(defbinary g/make-rectangular)
(defbinary g/make-polar)
(defunary g/real-part)
(defunary g/imag-part)
(defunary g/magnitude)
(defunary g/angle)
(defunary g/conjugate)

;; Generic Methods

(defmethod g/zero? [::v/function] [_] false)
(defmethod g/one? [::v/function] [_] false)
(defmethod g/identity? [::v/function] [_] false)
(defmethod g/zero-like [::v/function] [f] (zero-like f))
(defmethod g/one-like [::v/function] [f] (one-like f))
(defmethod g/identity-like [::v/function] [f] (identity-like f))
(defmethod g/exact? [::v/function] [f] (compose g/exact? f))

(defmethod g/freeze [::v/function] [f]
  (core/get @v/object-name-map f
            (cond
              (instance? MultiFn f)
              (if-let [m (get-method f [Keyword])]
                (m :name)
                f)

              #?@(:clj [(instance? AFunction f)
                        (:name (meta f) f)]

                  :cljs [(instance? MetaFn f)
                         (:name (.-meta f) f)])

              (var? f)
              (g/freeze @f)

              :else f)))

;; ## IPerturbed Implementation for Functions
;;
;; The following section, along with [[emmy.collection]] and [[emmy.dual]],
;; rounds out the implementations of [[emmy.dual/IPerturbed]] for native
;; Clojure(Script) data types. The function implementation is subtle, as
;; described by [Manzyuk et al. 2019](https://arxiv.org/pdf/1211.4892.pdf).
;; ([[emmy.derivative.calculus-test]], in the "Amazing Bug" sections,
;; describes the pitfalls at length.)
;;
;; [[emmy.dual]] describes how each in-progress perturbed variable in a
;; derivative is assigned a "tag" that accumulates the variable's partial
;; derivative.
;;
;; How do we interpret the case where `((D f) x)` produces a _function_?
;;
;; [Manzyuk et al. 2019](https://arxiv.org/pdf/1211.4892.pdf) extends `D` to
;; functions `f` of type $\mathbb{R}^n \rightarrow \alpha$, where
;;
;; $$\alpha::=\mathbb{R}^m \mid \alpha_{1} \rightarrow \alpha_{2}$$
;;
;; By viewing
;;
;; - `f` as a (maybe curried) multivariable function that _eventually_ must
;;   produce an $\mathbb{R}^m$
;; - The derivative `(D f)` as the partial derivative with respect to the first
;;   argument of `f`
;;
;; A 3-level nest of functions will respond to `D` just like the flattened,
;; non-higher-order version would respond to `(partial 0)`. In other words,
;; these two forms should evaluate to equivalent results:

^{:nextjournal.clerk/visibility {:result :hide}}
(comment
  (let [f (fn [x]
            (fn [y]
              (fn [z]
                (g/* x y z))))]
    ((((D f) 'x) 'y) 'z))
  ;;=> (* y z)

  (((partial 0) g/*) 'x 'y 'z))
;;=> (* y z)


;; To `extract-tangent` from a function, we need to compose the
;; `extract-tangent` operation with the returned function.
;;
;; The returned function needs to capture an internal reference to the
;; original [[emmy.dual/Dual]] input. This is true for any Functor-shaped return
;; value, like a structure or Map. However! There is a subtlety present with
;; functions that's not present with vectors or other containers.
;;
;; The difference with functions is that they take _inputs_. If you contrive a
;; situation where you can feed the original captured [[emmy.dual/Dual]] into
;; the returned function, this can trigger "perturbation confusion", where two
;; different layers try to extract the tangent corresponding to the SAME tag,
;; and one is left with nothing.
;;
;; If you engineer an
;; example (see [[emmy.calculus.derivative-test/amazing-bug]]) where:
;;
;; - this function takes another function, which then receives the closed-over
;;   `x` as an argument
;; - you pass this function to itself, so the closed-over `x` instances can both
;;   be multiplied
;;
;; Then your program isn't going to make any distinction between the instances
;; of `x`. They're both references to the same value.
;;
;; HOWEVER! `((D f) x)` returns a function which, when you eventually provide
;; all arguments, will return the sensitivity of `f` to the first argument `x`.
;;
;; If you perform the trick above, pass `((D f) x)` into itself, and the `x`
;; instances meet (multiply, say) - should final return value treat them as the
;; /same/ instance?
;;
;; Manzyuk et al. says _NO!_. If `((D f) x)` returns a function, that function
;; closes over:
;;
;; - the value of `x`
;; - an _intention_ to start the derivative-taking process on that isolated copy
;;   of `x` once the final argument is supplied.
;;
;; How does the implementation keep the values separate?
;;
;; ### Tag Replacement
;;
;; The key to the solution lives in [[extract-tangent-fn]], called on the result
;; of `((D f) x)` when `((D f) x)` produces a function. We have to armor the
;; returned function so that:
;;
;; - it extracts the originally-injected tag when someone eventually calls the
;;   function
;; - if some caller passes a new [[emmy.dual/Dual]] instance into the function,
;;   any tags in that [[emmy.dual/Dual]] will survive on their way back out...
;;   even if they happen to contain the originally-injected tag.
;;
;; We do this by:
;;
;; - replacing any instance of the original `tag` in the returned function's
;;   arguments with a temporary tag (let's call it `fresh`)
;; - calling the function and extracting the tangent component associated with
;;   `tag`, as requested (note now that the only instances of `tag` that can
;;   appear in the result come from variables captured in the function's
;;   closure)
;; - remapping `fresh` back to `tag` inside the remaining [[emmy.dual/Dual]]
;;   instance.
;;
;; This last step ensures that any tangent tagged with `tag` in the input can
;; make it back out without tangling with closure-captured `tag` instances that
;; some higher level might want.

(defn- extract-tangent-fn
  "Returns a new function that composes a 'tag extraction' step with `f`. The
  returned fn will

  - call the underlying `f`, producing `result`
  - return `(extract-tangent result tag)`

  If called within the scope of a function waiting for the same `tag`, the
  returned function will remap any instance of `tag` that appears in any
  differential argument passed to it to a private `fresh` tag, to prevent
  internal perturbation confusion. Any tangent components in the final result
  tagged with `fresh` will be remapped in the final result back to `tag`.

  If called _outside_ of a function waiting for `tag` no tag remapping will
  occur."
  [f tag mode]
  (-> (fn [& args]
        (if (d/tag-active? tag)
          (let [fresh (d/fresh-tag)]
            (-> (d/with-active-tag tag f (map #(d/replace-tag % tag fresh) args))
                (d/extract-tangent tag mode)
                (d/replace-tag fresh tag)))
          (-> (d/with-active-tag tag f args)
              (d/extract-tangent tag mode))))
      (with-arity (arity f))))

;; NOTE: that the tag-remapping that the docstring for `extract-tag-fn`
;; describes might _also_ have to apply to a functional argument!
;;
;; `replace-tag` on a function is meant to be a `replace-tag` call applied to
;; the function's _output_. To prevent perturbation confusion inside the
;; function, we perform a similar remapping of any occurrence of `tag` in the
;; function's arguments.

(defn- replace-tag-fn
  "Returns a new function that composes a 'tag replacement' step with `f`.

  If called within the scope of a function waiting for the same `tag`, the
  returned function will:

  - make a fresh tag, and replace all `old` tags with `fresh` in the inputs
  - call `f`, producing `result`
  - return `(replace-tag result old new)`
  - remap any tangent component in the result tagged with `fresh` back to `old`.

  If called _outside_ of a function waiting for `tag`, the returned function
  will apply `f` to its arguments and call `(replace-tag result old new)` with
  no tag-rerouting."
  [f old new]
  (-> (fn [& args]
        (if (d/tag-active? old)
          (let [fresh (d/fresh-tag)
                args  (map #(d/replace-tag % old fresh) args)]
            (-> (apply f args)
                (d/replace-tag old new)
                (d/replace-tag fresh old)))
          (-> (apply f args)
              (d/replace-tag old new))))
      (with-arity (arity f))))

;; ## Protocol Implementation
;;
;; The implementation for functions handles functions, multimethods, and, in
;; ClojureScript, [[MetaFn]] instances. Metadata in the original function is
;; preserved through tag replacement and extraction.

(extend-protocol d/IPerturbed
  MultiFn
  (replace-tag [f old new] (replace-tag-fn f old new))
  (extract-tangent [f tag mode] (extract-tangent-fn f tag mode))
  (extract-id [f id] (comp #(d/extract-id % id) f))

  #?@(:clj
      [;; In Clojure, metadata can live directly on function objects.
       Fn
       (replace-tag [f old new] (replace-tag-fn f old new))
       (extract-tangent [f tag mode] (extract-tangent-fn f tag mode))
       (extract-id [f id] (comp #(d/extract-id % id) f))]

      :cljs
      [;; In Clojurescript, we arrange for metadata to live directly on
       ;; function objects by setting a special property and implementing
       ;; IMeta: see [[emmy.value]].
       function
       (replace-tag [f old new] (replace-tag-fn f old new))
       (extract-tangent [f tag mode] (extract-tangent-fn f tag mode))
       (extract-id [f id] (comp #(d/extract-id % id) f))

       ;; The official way to get metadata onto a function in Clojurescript
       ;; is to promote the fn to an AFn-implementing object and store the
       ;; metadata on a directly-visible object property, which we also
       ;; support, although such objects are not naively callable in JavaScript
       MetaFn
       (replace-tag [f old new]
                    (replace-tag-fn (.-afn f) old new))
       (extract-tangent [f tag mode]
                        (extract-tangent-fn (.-afn f) tag mode))
       (extract-id [f id] (comp #(d/extract-id % id) (.-afn f)))]))
