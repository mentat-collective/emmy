#_"SPDX-License-Identifier: GPL-3.0"

^#:nextjournal.clerk
{:toc true
 :visibility :hide-ns}
(ns emmy.differential
  "This namespace contains an implementation of [[Dual]], a type that forms the
  basis for the forward-mode automatic differentiation implementation in emmy.

  See [[emmy.calculus.derivative]] for a fleshed-out derivative
  implementation using [[Dual]]."
  (:refer-clojure :exclude [compare])
  (:require [emmy.function]  ;; for the side effect of making kind: MultiFn -> ::v/function
            [emmy.generic :as g]
            [emmy.util :as u]
            [emmy.value :as v]))

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
;; Emmy, like [[+]], [[*]], [[emmy.generic/sin]] and friends. It turns out that
;; we can take the derivatives of large, complicated functions by combining the
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
    "Returns true if the supplied object has some known non-zero tangent to be
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
  nil
  (perturbed? [_] false)
  (replace-tag [_ _ _] nil)
  (extract-tangent [_ _] 0)

  #?(:clj Object :cljs default)
  (perturbed? [_] false)
  (replace-tag [this _ _] this)
  (extract-tangent [this _] (g/zero-like this)))

;; ## Dual Implementation
;;
;; We now have a template for how to implement `derivative`. What's left? We
;; need a dual number type that we can build and split back out into primal and
;; tangent components, given some tag. We'll call this type a [[Dual]].
;;
;; Since the only use of a tag is to distinguish each unnamed $\varepsilon_n$,
;; we'll assign a new, unique positive integer for each new tag:

(let [next-tag (atom -1)]
  (defn fresh-tag
    "Returns a new, unique tag for use by a perturbation in an automatic
  differentiation pass."
    []
    (swap! next-tag inc)))

;; A [[Dual]] will respond to [[emmy.value/kind]] with `::dual`. Because we
;; want [[Dual]] instances to work in any place that real numbers or
;; symbolic argument work, let's make `::dual` derive from
;; `::emmy.value/scalar`:

(derive ::dual ::v/scalar)

;; Now the actual type.

(declare compare equiv)

(deftype Dual [tag primal tangent]
  IPerturbed
  (perturbed? [_] true)

  (replace-tag [this old new]
    (if (= old tag)
      (Dual. new primal tangent)
      this))

  (extract-tangent [_ t]
    (if (= t tag) tangent 0))

  v/IKind
  (kind [_] ::dual)

  Object
  ;; Comparing [[Dual]] objects using `equals` defaults to [[equiv]], which
  ;; compares instances only using primal components. If you want to compare two
  ;; instances using both primal and tangent components, see [[eq]].
  #?(:clj (equals [a b] (equiv a b)))
  #?(:cljs (valueOf [_] (.valueOf primal)))
  (toString [_]
    (str "#emmy.tape.Dual"
         {:tag tag
          :primal primal
          :tangent tangent}))

  #?@(:clj
      ;; This one is slightly subtle. To participate in control flow operations,
      ;; like comparison with both [[Dual]] and non-[[Dual]] numbers, [[Dual]]
      ;; instances should compare using ONLY their primal terms. This means that
      ;; comparison will totally ignore any difference in tangents or tags.
      [Comparable
       (compareTo [a b] (compare a b))]

      :cljs
      [IEquiv
       (-equiv [a b] (equiv a b))

       IComparable
       (-compare [a b]  (compare a b))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer (.toString x)))]))

#?(:clj
   (defmethod print-method Dual
     [^Dual s ^java.io.Writer w]
     (.write w (.toString s))))

(defn dual?
  "Returns true if the supplied object is an instance of [[Dual]], false
  otherwise."
  [dx]
  (instance? Dual dx))

(defn tag
  "If `dx` is an instance of [[Dual]] returns the `tag` component. Else, acts
  as nil."
  [dx]
  (when (dual? dx)
    (.-tag ^Dual dx)))

(defn primal-tangent-pair
  "Returns a pair of the primal and tangent components of the supplied `dx`, with
  respect to the supplied `tag`. See the docs for [[primal]]
  and [[tangent]] for more details.

  [[primal-tangent-pair]] is equivalent to

  `[([[primal]] dx tag) ([[tangent]] dx tag)]`

  but slightly more efficient if you need both."
  ([dx]
   (if (dual? dx)
     [(.-primal ^Dual dx) (.-tangent ^Dual dx)]
     [dx 0]))
  ([dx t]
   (if (and (dual? dx) (= t (tag dx)))
     [(.-primal ^Dual dx) (.-tangent ^Dual dx)]
     [dx 0])))

(defn primal
  "If `dx` is an instance of [[Dual]] returns the `primal` component. Else, acts
  as identity.

  If the optional `tag` is supplied, [[primal-part]] acts as identity
  for [[Dual]] instances with a non-matching tag."
  ([dx]
   (-> (primal-tangent-pair dx)
       (nth 0)))
  ([dx tag]
   (-> (primal-tangent-pair dx tag)
       (nth 0))))

(defn deep-primal
  "Version of [[primal]] that will descend recursively into any [[Dual]] instance
  returned by [[primal]] until encountering a non-[[Dual]].

  Given a non-[[Dual]], acts as identity."
  [dx]
  (if (dual? dx)
    (recur (.-primal ^Dual dx))
    dx))

(defn tangent
  "If `dx` is an instance of [[Dual]] returns the `tangent` component. Else, returns 0.

  If the optional `tag` is supplied, [[primal-part]] returns 0 for [[Dual]]
  instances with a non-matching tag."
  ([dx]
   (-> (primal-tangent-pair dx)
       (nth 1)))
  ([dx tag]
   (-> (primal-tangent-pair dx tag)
       (nth 1))))

;; ## Constructor

(defn bundle-element
  "Returns a new [[Dual]] object with the supplied `primal` and `tangent`
  components, and the supplied internal `tag` that this [[Dual]] will
  carry around to prevent perturbation confusion.

  If the `tangent` component is `0`, acts as identity on `primal`. `tangent`
  defaults to 1.

  `tag` defaults to a side-effecting call to [[fresh-tag]]; you can retrieve
  this unknown tag by calling [[tag]] on the returned [[Dual]]."
  ([primal]
   (bundle-element primal 1 (fresh-tag)))
  ([primal tag]
   (bundle-element primal 1 tag))
  ([primal tangent tag]
   {:pre [(v/scalar? primal)]}
   (if (g/zero? tangent)
     primal
     (->Dual tag primal tangent))))

;; ## Tag API
;;
;; These first two functions create a way to globally declare, via a dynamic
;; binding, the stack of tags that are currently in play. If three nested
;; derivatives are being taken, [[*active-tags*]] will contain three entries
;; from a perspective inside the function at the deepest level.
;;
;; The [[IPerturbed]] implementation for functions uses this information to
;; determine whether or not to use [[replace-tag]] to protect its tag from
;; perturbation confusion. If some higher level is not trying to extract the
;; same tag, there's no need.

(def ^:dynamic *active-tags* ())

(defn with-active-tag
  "Like `apply`, but conj-es `tag` onto the dynamic variable [[*active-tags*]]
  inside the scope of `f`.

  Returns the result of applying `f` to `args`."
  [tag f args]
  (binding [*active-tags* (cons tag *active-tags*)]
    (apply f args)))

(defn tag-active?
  "Returns true if `tag` is an element of [[*active-tags*]] (and therefore pending
  for extraction by some nested derivative), false otherwise."
  [tag]
  (boolean
   (some #{tag} *active-tags*)))

(defn inner-tag
  "Given any number of `tags`, returns the tag most recently bound
  via [[with-active-tag]] (i.e., the tag connected with the _innermost_ call
  to [[with-active-tag]]).

  If none of the tags are bound, returns `(apply max tags)`."
  [& tags]
  (or (some (apply hash-set tags)
            *active-tags*)
      (apply max tags)))

(defn tag+perturbation
  "Given any number of [[Dual]] instances `dxs`, returns a pair of the form

  [<tag> <dual number>]

  containing the tag and instance of [[Dual]] associated with the inner-most
  call to [[with-active-tag]] in the current call stack.

  If none of `dxs` has an active tag, returns `nil`."
  ([& dxs]
   (let [m (into {} (mapcat
                     (fn [dx]
                       (when-let [t (tag dx)]
                         {t dx})))
                 dxs)]
     (when (seq m)
       (let [tag (apply inner-tag (keys m))]
         [tag (m tag)])))))

;; ## Comparison, Control Flow
;;
;; Functions like `=`, `<` and friends don't have derivatives; instead, they're
;; used for control flow inside of Clojure functions. To play nicely with these
;; functions, the [[Dual]] API exposes a number of methods for comparing
;; numbers on ONLY their finite parts.
;;
;; Why? If `x` is a [[Dual]] instance, `(< x 10)` needs to return true whenever
;; a non-[[Dual]] `x` would return true. To make this work, these operations
;; look only at the [[primal]].
;;
;; HOWEVER! [[g/one?]] and [[g/zero?]] are examples of Emmy functions that
;; are used to skip operations that we _want_ to happen, like multiplication.
;;
;; `(g/* x y)` will return `y` if `(g/one? x)` is true... but to propagate the
;; derivative through we need this multiplication to occur. The compromise is:
;;
;; - [[g/zero?]], [[g/one?]] and [[g/identity?]] return true only when
;;  [[tangent]] is zero and the [[primal]] is either [[g/one?]] or [[g/zero?]]
;;   respectively
;; - [[eq]] and [[compare-full]] similarly looks at [[primal]] and [[tangent]]
;;   in the [[Dual]] supplied to both sides
;;
;; while:
;;
;; - [[equiv]] and [[compare]] only examine the [[primal]] of either side.

(defn ^:no-doc one?
  "Returns true if the supplied instance has a [[primal]] part that responds true
  to [[emmy.value/one?]], and zero coefficients on its tangent component; false
  otherwise.

  NOTE: This means that [[one?]] will not do what you expect as a conditional
  inside some function. If you want to branch inside some function you're taking
  the derivative of, prefer `(= 1 dx)`. This will only look at
  the [[primal]] and ignore the value of the [[tangent]]."
  [dx]
  (let [[p t] (primal-tangent-pair dx)]
    (and (g/one? p)
         (g/zero? t))))

(defn ^:no-doc identity?
  "Returns true if the supplied instance has a [[primal]] that responds true
  to [[emmy.value/identity?]], and a zero [[tangent]], false otherwise.

  NOTE: This means that [[identity?]] will not do what you expect as a
  conditional inside some function. If you want to branch inside some function
  you're taking the derivative of, prefer `(= <identity element> dx)`. This will
  only look at the [[primal]] and ignore the value of the [[tangent]]."
  [dx]
  (let [[p t] (primal-tangent-pair dx)]
    (and (g/identity? p)
         (g/zero? t))))

(defn eq
  "For non-differentials, this is identical to [[emmy.value/=]].
  For [[Dual]] instances, equality acts on tangent components too.

  If you want to ignore the tangent components, use [[equiv]]."
  ([_] true)
  ([a b]
   (v/= (primal-tangent-pair a)
        (primal-tangent-pair b)))
  ([a b & more]
   (if (eq a b)
     (if (next more)
       (recur b (first more) (next more))
       (eq b (first more)))
     false)))

(defn compare-full
  "Comparator that compares [[Dual]] instances with each other or
  non-differentials using all tangent terms each instance. Matches the response
  of [[eq]].

  Acts as [[emmy.value/compare]] for non-differentials."
  [a b]
  (v/compare
   (primal-tangent-pair a)
   (primal-tangent-pair b)))

(defn equiv
  "Returns true if all of the supplied objects have equal [[primal]]s, false
  otherwise.

  Use [[equiv]] if you want to compare scalars with
  [[Dual]]s and ignore the tangent. If you _do_ want to take the tangent into
  account, prefer [[eq]]."
  ([_] true)
  ([a b]
   (v/= (primal a)
        (primal b)))
  ([a b & more]
   (if (equiv a b)
     (if (next more)
       (recur b (first more) (next more))
       (equiv b (first more)))
     false)))

(defn compare
  "Comparator that compares [[Dual]] instances with each other or
  non-differentials using only the [[primal]] of each instance. Matches the
  response of [[equiv]].

  Acts as [[emmy.value/compare]] for non-differentials."
  [a b]
  (v/compare
   (primal a)
   (primal b)))

;; ## Chain Rule and Lifted Functions
;;
;; Finally, we come to the heart of it! [[lift-1]] and [[lift-2]] "lift", or
;; augment, unary or binary functions with the ability to
;; handle [[Dual]] instances in addition to whatever other types they
;; previously supported.
;;
;; These functions are implementations of the single and multivariable Taylor
;; series expansion methods discussed at the beginning of the namespace.
;;
;; There is yet another subtlety here, noted in the docstrings below. [[lift-1]]
;; and [[lift-2]] really are able to lift functions like [[clojure.core/+]] that
;; can't accept [[Dual]]s. But the first-order derivatives that you have
;; to supply _do_ have to be able to take [[Dual]] instances.
;;
;; This is because the [[tangent]] of [[Dual]] might still be a [[Dual]], and
;; for `Df` to handle this we need to be able to take the second-order
;; derivative.
;;
;; Magically this will all Just Work if you pass an already-lifted function, or
;; a function built out of already-lifted components, as `df:dx` or `df:dy`.

(defn lift-1
  "Given:

  - some unary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument

  Returns a new unary function that operates on both the original type of `f`
  and [[Dual]] instances.

  If called without `df:dx`, `df:dx` defaults to `(f :dfdx)`; this will return
  the derivative registered to a generic function defined
  with [[emmy.util.def/defgeneric]].

  NOTE: `df:dx` has to ALREADY be able to handle [[Dual]] instances. The best
  way to accomplish this is by building `df:dx` out of already-lifted functions,
  and declaring them by forward reference if you need to."
  ([f]
   (if-let [df:dx (f :dfdx)]
     (lift-1 f df:dx)
     (u/illegal "No df:dx supplied for `f` or registered generically.")))
  ([f df:dx]
   (fn call [x]
     (if-not (dual? x)
       (f x)
       (let [[px tx] (primal-tangent-pair x)
             primal  (call px)
             tangent (g/* (df:dx px) tx)]
         (bundle-element primal tangent (tag x)))))))

(defn lift-2
  "Given:

  - some binary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument
  - a function `df:dy`, similar to `df:dx` for the second arg

  Returns a new binary function that operates on both the original type of `f`
  and [[Dual]] instances.

  NOTE: `df:dx` and `df:dy` have to ALREADY be able to handle [[Dual]]
  instances. The best way to accomplish this is by building `df:dx` and `df:dy`
  out of already-lifted functions, and declaring them by forward reference if
  you need to."
  ([f]
   (let [df:dx (f :dfdx)
         df:dy (f :dfdy)]
     (if (and df:dx df:dy)
       (lift-2 f df:dx df:dy)
       (u/illegal "No df:dx, df:dy supplied for `f` or registered generically."))))
  ([f df:dx df:dy]
   (fn call [x y]
     (if-let [[tag _] (tag+perturbation x y)]
       (let [[xe dx] (primal-tangent-pair x tag)
             [ye dy] (primal-tangent-pair y tag)
             primal  (call xe ye)
             tangent (g/+ (if (g/numeric-zero? dx)
                            dx
                            (g/* (df:dx xe ye) dx))
                          (if (g/numeric-zero? dy)
                            dy
                            (g/* (df:dy xe ye) dy)))]
         (bundle-element primal tangent tag))
       (f x y)))))

(defn lift-n
  "Given:

  - some function `f` that can handle 0, 1 or 2 arguments
  - `df:dx`, a fn that returns the derivative wrt the single arg in the unary case
  - `df:dx1` and `df:dx2`, fns that return the derivative with respect to the
    first and second args in the binary case

  Returns a new any-arity function that operates on both the original type of
  `f` and [[Dual]] instances.

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
;; Armed with [[lift-1]] and [[lift-2]], we can install [[Dual]] into
;; the Emmy generic arithmetic system.
;;
;; Any function built out of these components will work with
;; the [[emmy.calculus.derivative/D]] operator.

(defn- defunary
  "Given:

  - a generic unary multimethod `generic-op`
  - optionally, a corresponding single-arity lifted function
    `differential-op` (defaults to `(lift-1 generic-op)`)

  installs an appropriate unary implementation of `generic-op` for `::dual`
  instances."
  ([generic-op]
   (defunary generic-op (lift-1 generic-op)))
  ([generic-op differential-op]
   (defmethod generic-op [::dual] [a] (differential-op a))))

(defn- defbinary
  "Given:

  - a generic binary multimethod `generic-op`
  - optionally, a corresponding 2-arity lifted function
    `differential-op` (defaults to `(lift-2 generic-op)`)

  installs an appropriate binary implementation of `generic-op` between `::dual`
  and `::v/scalar` instances."
  ([generic-op]
   (defbinary generic-op (lift-2 generic-op)))
  ([generic-op differential-op]
   (doseq [signature [[::dual ::dual]
                      [::v/scalar ::dual]
                      [::dual ::v/scalar]]]
     (defmethod generic-op signature [a b] (differential-op a b)))))

(defn ^:no-doc by-primal
  "Given some unary or binary function `f`, returns an augmented `f` that acts on
  the primal entries of any [[Dual]] arguments encountered, irrespective of tag.

  Given a [[Dual]] with a [[Dual]] in its [[primal]] part, the returned `f` will
  recursively descend until it hits a non-[[Dual]]."
  [f]
  (fn
    ([x] (f (deep-primal x)))
    ([x y] (f (deep-primal x)
              (deep-primal y)))))

;; And now we're off to the races. The rest of the namespace
;; provides [[defunary]] and [[defbinary]] calls for all of the generic
;; operations for which we know how to declare partial derivatives.

;; First, install `equiv` as to perform proper equality between `Dual`
;; instances and scalars. `equiv` compares on only the finite part, not the
;; differential parts.

(defbinary g/add)
(defunary g/negate)
(defbinary g/sub)

(let [mul  (lift-2 g/mul)]
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
         (str "Derivative of emmy.generic/"
              f-name " undefined at integral points."))
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
(defunary g/negative? (by-primal g/negative?))
(defunary g/infinite? (by-primal g/infinite?))


(defunary g/zero?
  (fn [dx]
    (and (g/zero? (primal dx))
         (g/zero? (tangent dx)))))

(defunary g/one?
  (fn [dx]
    (and (g/one? (primal dx))
         (g/zero? (tangent dx)))))

(defunary g/identity?
  (fn [dx]
    (and (g/identity? (primal dx))
         (g/zero? (tangent dx)))))

(defmethod g/zero-like [::dual] [_] 0)
(defmethod g/one-like [::dual] [_] 1)
(defmethod g/identity-like [::dual] [_] 1)
(defmethod g/freeze [::dual] [d]
  `[~'Dual
    ~(tag d)
    ~(g/freeze (primal d))
    ~(g/freeze (tangent d))])

(defmethod g/simplify [::dual] [^Dual d]
  (Dual. (.-tag d)
         (g/simplify (.-primal d))
         (g/simplify (.-tangent d))))
