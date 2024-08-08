^#:nextjournal.clerk
{:toc true
 :visibility :hide-ns}
(ns emmy.autodiff
  (:require [emmy.dual :as d]
            [emmy.generic :as g]
            [emmy.tape :as t]
            [emmy.util :as u]
            [emmy.value :as v]))

;; ## Implementations

(defn tag-of
  "More permissive version of [[emmy.tape/tape-tag]] that returns `nil` when
  passed a non-perturbation."
  [x]
  (cond (t/tape? x) (t/tape-tag x)
        (d/dual? x) (d/tag x)
        :else       nil))

(defn primal-of
  "More permissive version of [[emmy.tape/tape-primal]] that returns `v` when passed a
  non-perturbation."
  ([v]
   (primal-of v (tag-of v)))
  ([v tag]
   (cond (t/tape? v) (t/tape-primal v tag)
         (d/dual? v) (d/primal v tag)
         :else       v)))

(defn inner-tag
  "Given any number of `tags`, returns the tag most recently bound
  via [[with-active-tag]] (i.e., the tag connected with the _innermost_ call
  to [[with-active-tag]]).

  If none of the tags are bound, returns `(apply max tags)`."
  [& tags]
  (or (some (apply hash-set tags)
            d/*active-tags*)
      (apply max tags)))

(defn tag+perturbation
  "Given any number of `dxs`, returns a pair of the form

  [<tag> <tape-or-dual-number>]

  containing the tag and instance of [[emmy.dual/Dual]] or [[TapeCell]]
  associated with the inner-most call to [[with-active-tag]] in the current call
  stack.

  If none of `dxs` has an active tag, returns `nil`."
  ([& dxs]
   (let [xform (map
                (fn [dx]
                  (when-let [t (tag-of dx)]
                    [t dx])))
         m     (into {} xform dxs)]
     (when (seq m)
       (let [tag (apply inner-tag (keys m))]
         [tag (m tag)])))))

(defn deep-primal
  "Version of [[tape-primal]] that will descend recursively into any perturbation
  instance returned by [[tape-primal]] or [[emmy.dual/primal]] until
  encountering a non-perturbation.

  Given a non-perturbation, acts as identity."
  ([v]
   (cond (t/tape? v) (recur (t/tape-primal v))
         (d/dual? v) (recur (d/primal v))
         :else       v)))

;; ## Lifted Functions

;; [[lift-1]] and [[lift-2]] "lift", or augment, unary or binary functions with
;; the ability to handle [[emmy.dual/Dual]] and [[TapeCell]] instances in
;; addition to whatever other types they previously supported.
;;
;; Forward-mode support for [[emmy.dual/Dual]] is an implementation of the
;; single and multivariable Taylor series expansion methods discussed at the
;; beginning of [[emmy.dual]].
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

;; There is a subtlety here, noted in the docstrings below. [[lift-1]]
;; and [[lift-2]] really are able to lift functions like [[clojure.core/+]] that
;; can't accept [[emmy.dual/Dual]] and [[TapeCell]]s. But the first-order
;; derivatives that you have to supply _do_ have to be able to take instances of
;; these types.
;;
;; This is because, for example, the [[emmy.dual/tangent]] of [[emmy.dual/Dual]]
;; might still be an [[emmy.dual/Dual]], and will hit the first-order derivative
;; via the chain rule.
;;
;; Magically this will all Just Work if you pass an already-lifted function, or
;; a function built out of already-lifted components, as `df:dx` or `df:dy`.

(defn lift-1
  "Given:

  - some unary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument

  Returns a new unary function that operates on both the original type of
  `f`, [[TapeCell]] and [[emmy.dual/Dual]] instances.

  If called without `df:dx`, `df:dx` defaults to `(f :dfdx)`; this will return
  the derivative registered to a generic function defined
  with [[emmy.util.def/defgeneric]].

  NOTE: `df:dx` has to ALREADY be able to handle [[TapeCell]]
  and [[emmy.dual/Dual]] instances. The best way to accomplish this is by
  building `df:dx` out of already-lifted functions, and declaring them by
  forward reference if you need to."
  ([f]
   (if-let [df:dx (f :dfdx)]
     (lift-1 f df:dx)
     (u/illegal
      "No df:dx supplied for `f` or registered generically.")))
  ([f df:dx]
   (fn call [x]
     (cond (t/tape? x)
           (let [primal (t/tape-primal x)]
             (t/make (t/tape-tag x)
                     (call primal)
                     [[x (df:dx primal)]]))

           (d/dual? x)
           (let [[px tx] (d/primal-tangent-pair x)
                 primal  (call px)
                 tangent (g/* (df:dx px) tx)]
             (d/bundle-element primal tangent (d/tag x)))

           :else (f x)))))

(defn lift-2
  "Given:

  - some binary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument
  - a function `df:dy`, similar to `df:dx` for the second arg

  Returns a new binary function that operates on both the original type of
  `f`, [[TapeCell]] and [[emmy.dual/Dual]] instances.

  NOTE: `df:dx` and `df:dy` have to ALREADY be able to handle [[TapeCell]]
  and [[emmy.dual/Dual]] instances. The best way to accomplish this is
  by building `df:dx` and `df:dy` out of already-lifted functions, and declaring
  them by forward reference if you need to."
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
               (let [[xe dx] (d/primal-tangent-pair x tag)
                     [ye dy] (d/primal-tangent-pair y tag)
                     primal  (call xe ye)
                     tangent (g/+ (if (g/numeric-zero? dx)
                                    dx
                                    (g/* (df:dx xe ye) dx))
                                  (if (g/numeric-zero? dy)
                                    dy
                                    (g/* (df:dy xe ye) dy)))]
                 (d/bundle-element primal tangent tag)))

             (operate-reverse [tag]
               (let [primal-x  (t/tape-primal x tag)
                     primal-y  (t/tape-primal y tag)
                     partial-x (if (and (t/tape? x) (= tag (t/tape-tag x)))
                                 [[x (df:dx primal-x primal-y)]]
                                 [])
                     partial-y (if (and (t/tape? y) (= tag (t/tape-tag y)))
                                 [[y (df:dy primal-x primal-y)]]
                                 [])]

                 (t/make tag
                         (call primal-x primal-y)
                         (into partial-x partial-y))))]
       (if-let [[tag dx] (tag+perturbation x y)]
         (cond (t/tape? dx) (operate-reverse tag)
               (d/dual? dx) (operate-forward tag)
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
  `f`, [[TapeCell]] and [[emmy.dual/Dual]] instances.

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
;; Armed with [[lift-1]] and [[lift-2]], we can install [[TapeCell]] into
;; the Emmy generic arithmetic system.

(defn- defunary
  "Given:

  - a generic unary multimethod `generic-op`
  - optionally, a corresponding single-arity lifted function
    `differential-op` (defaults to `(lift-1 generic-op)`)

  installs an appropriate unary implementation of `generic-op` for `::tape` and
  `:emmy.dual/dual` instances."
  ([generic-op]
   (defunary generic-op (lift-1 generic-op)))
  ([generic-op differential-op]
   (defmethod generic-op [::d/dual] [a] (differential-op a))
   (defmethod generic-op [::t/tape] [a] (differential-op a))))

(defn- defbinary
  "Given:

  - a generic binary multimethod `generic-op`
  - optionally, a corresponding 2-arity lifted function
    `differential-op` (defaults to `(lift-2 generic-op)`)

  installs an appropriate binary implementation of `generic-op` between
  `::t/tape`, `:emmy.dual/dual` and `:emmy.value/scalar` instances."
  ([generic-op]
   (defbinary generic-op (lift-2 generic-op)))
  ([generic-op differential-op]
   (doseq [signature [[::t/tape ::t/tape]
                      [::d/dual ::d/dual]
                      [::t/tape ::d/dual]
                      [::d/dual ::t/tape]
                      [::v/scalar ::t/tape]
                      [::v/scalar ::d/dual]
                      [::t/tape ::v/scalar]
                      [::d/dual ::v/scalar]]]
     (defmethod generic-op signature [a b] (differential-op a b)))))

(defn ^:no-doc by-primal
  "Given some unary or binary function `f`, returns an augmented `f` that acts on
  the primal entries of any perturbed arguments encountered, irrespective of
  tag."
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
(defunary g/zero?
  (let [zero-p? (by-primal g/zero?)]
    (fn [dx]
      (if (t/tape? dx)
        (zero-p? dx)
        (let [[p t] (d/primal-tangent-pair dx)]
          (and (g/zero? p)
               (g/zero? t)))))))

(defunary g/one?
  (let [one-p? (by-primal g/one?)]
    (fn [dx]
      (if (t/tape? dx)
        (one-p? dx)
        (d/one? dx)))))

(defunary g/identity?
  (let [identity-p? (by-primal g/identity?)]
    (fn [dx]
      (if (t/tape? dx)
        (identity-p? dx)
        (d/identity? dx)))))

(defunary g/negative? (by-primal g/negative?))
(defunary g/infinite? (by-primal g/infinite?))
