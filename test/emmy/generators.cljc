#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.generators
  "test.check generators for the various types in the emmy project."
  (:refer-clojure :exclude [bigint biginteger double long symbol])
  (:require [clojure.core :as core]
            [clojure.test.check.generators :as gen]
            #?(:cljs [emmy.bigfraction :refer [Fraction]])
            [emmy.complex :as c]
            #?(:cljs [emmy.complex.impl :refer [Complex]])
            [emmy.differential :as d]
            [emmy.generic :as g]
            [emmy.matrix :as m]
            [emmy.modint :as mi]
            [emmy.numsymb :as sym]
            [emmy.polynomial :as poly]
            [emmy.polynomial.exponent :as xpt]
            [emmy.quaternion :as quat
             #?@(:cljs [:refer [Quaternion]])]
            [emmy.ratio :as r]
            [emmy.rational-function :as rf]
            [emmy.series :as ss]
            [emmy.structure :as s]
            [emmy.tape :as tape]
            [emmy.util :as u]
            [emmy.util.vector-set :as vs]
            [emmy.value :as v]
            [same.ish :as si])
  #?(:clj
     (:import (clojure.lang Symbol)
              (emmy.structure Structure)
              (emmy.quaternion Quaternion)
              (emmy.complex.impl Complex))))

(def bigint
  "js/BigInt in cljs, clojure.lang.BigInt in clj."
  #?(:cljs
     (gen/fmap u/bigint gen/small-integer)
     :clj
     gen/size-bounded-bigint))

(def biginteger
  #?(:cljs bigint
     :clj (gen/fmap u/biginteger bigint)))

(def native-integral
  "non-floating-point integers on cljs, Long on clj."
  gen/small-integer)

(def long
  "goog.math.Long in cljs,
  java.lang.Long in clj."
  #?(:clj gen/large-integer
     :cljs (gen/fmap u/long gen/large-integer)))

(def integer
  "goog.math.Integer in cljs, java.lang.Integer in clj."
  (gen/fmap u/int gen/small-integer))

(defn reasonable-double
  ([] (reasonable-double {}))
  ([{:keys [min max excluded-lower excluded-upper]
     :or {min -10e5
          max 10e5
          excluded-lower -1e-4
          excluded-upper 1e-4}}]
   (gen/one-of [(gen/double* {:infinite? false
                              :NaN? false
                              :min min
                              :max excluded-lower})
                (gen/double* {:infinite? false
                              :NaN? false
                              :min excluded-upper
                              :max max})])))

(defn reasonable-complex
  "A complex number in the unit square, bounded away from the axes,
  in order to avoid branch points"
  []
  (let [bounds {:min -1 :max 1 :excluded-lower -1e-3 :excluded-upper 1e-3}]
    (gen/let [re (reasonable-double bounds)
              im (reasonable-double bounds)]
      (c/complex re im))))

(defn inexact-double
  ([] (inexact-double {}))
  ([opts]
   (->> (reasonable-double opts)
        (gen/fmap (fn [x]
                    (if (g/exact? x)
                      (+ x 0.5)
                      x))))))

(def small-integral
  (gen/one-of [native-integral long integer]))

(def any-integral
  (gen/one-of [small-integral
               bigint
               #?@(:clj [biginteger])]))

(def ratio
  "Generates a small ratio (or integer) using gen/small-integer. Shrinks
  toward simpler ratios, which may be larger or smaller."
  (gen/fmap
   (fn [[a b]] (r/rationalize a b))
   (gen/tuple gen/small-integer (gen/fmap inc gen/nat))))

(def big-ratio
  (gen/let [n bigint
            d bigint]
    (let [d (if (g/zero? d)
              (u/bigint 1)
              d)]
      (r/rationalize n d))))

(def rational
  (gen/one-of [any-integral ratio]))

(def real-without-ratio
  (gen/one-of [any-integral (reasonable-double)]))

(def real
  (gen/one-of
   [any-integral ratio (reasonable-double)]))

(defn reasonable-real [bound]
  (let [bound    (core/long bound)
        integral (gen/fmap
                  #(g/remainder % bound)
                  any-integral)]
    (gen/one-of [integral (reasonable-double
                           {:min (- bound)
                            :max bound})])))

(def complex
  (gen/let [r (reasonable-double)
            i (reasonable-double)]
    (c/complex r i)))

(def number
  (gen/one-of [real complex]))

;; ## Quaternions

(defn quaternion
  ([] (quaternion (reasonable-double)))
  ([coeff-gen]
   (gen/let [r coeff-gen
             i coeff-gen
             j coeff-gen
             k coeff-gen]
     (quat/make r i j k))))

;; ## Modular Arithmetic

(defn- not-zero [x]
  (if (zero? x) 1 x))

(defn modint
  ([] (modint 30))
  ([modulus-max]
   (gen/let [i gen/small-integer
             m (gen/fmap
                (fn [i]
                  (not-zero
                   (mod i modulus-max)))
                gen/small-integer)]
     (mi/make i m))))

;; ## Symbolic

(def symbol
  "generates symbols that won't clash with any of the symbols that generic
  operations freeze to, like `*`, `+`, `-`, `/` etc."
  (let [special-syms (set (keys @#'sym/symbolic-operator-table))]
    (gen/such-that (complement special-syms)
                   gen/symbol)))

;; ## Structure Generators

(defn- recursive
  "Similar to `gen/recursive-gen`, but guaranteed to produce containerized
  items (no primitives will pop out)."
  [container elem]
  (container
   (gen/recursive-gen
    container elem)))

(defn structure1*
  "Returns a generator that produces structures of the supplied orientation with
  elements chosen via `elem-gen`. All extra args are passed along to
  `gen/vector`."
  [orientation elem-gen & args]
  {:pre [(s/valid-orientation? orientation)]}
  (gen/fmap (partial s/make orientation)
            (apply gen/vector elem-gen args)))

(defn up1
  "Returns a generator that produces `up` structures of elements chosen via
  `elem-gen`. All extra args are passed along to `gen/vector`."
  [elem-gen & args]
  (apply structure1* ::s/up elem-gen args))

(defn down1
  "Returns a generator that produces `down` structures of elements chosen via
  `elem-gen`. All extra args are passed along to `gen/vector`."
  [elem-gen & args]
  (apply structure1* ::s/down elem-gen args))

(defn structure1
  "Returns a generator that produces `up` or `down` structures of elements chosen
  via `elem-gen`. All extra args are passed along to `gen/vector`."
  [elem-gen & args]
  (gen/one-of [(apply up1 elem-gen args)
               (apply down1 elem-gen args)]))

(defn up
  "Returns a generator that produces nested `up` structures of either further
  `up`s, or primitive elements chosen via `elem-gen`. All extra args are passed
  along to `gen/vector`."
  [elem-gen & args]
  (recursive #(apply up1 % args) elem-gen))

(defn down
  "Returns a generator that produces nested `down` structures of either further
  `down`s, or primitive elements chosen via `elem-gen`. All extra args are passed
  along to `gen/vector`."
  [elem-gen & args]
  (recursive #(apply down1 % args) elem-gen))

(defn structure
  "Returns a generator that produces nested `up` or `down` structures of either
  further containers, or primitive elements chosen via `elem-gen`. All extra
  args are passed along to `gen/vector`."
  [elem-gen & args]
  (recursive #(apply structure1 % args) elem-gen))

(def orientation
  "Returns a generator that produces a valid structure orientation."
  (gen/elements [::s/up ::s/down]))

;; ## Matrices

(defn matrix
  "Returns a generator that produces a matrix of the specified dimensions,
  populated with entries from `entry-gen` (if supplied.)

  `entry-gen` defaults to [[ratio]]."
  ([rows cols]
   (matrix rows cols ratio))
  ([rows cols entry-gen]
   (gen/fmap m/by-rows*
             (-> (gen/vector entry-gen rows)
                 (gen/vector cols)))))

(defn square-matrix
  "Returns a generator that produces a square matrix of dimension `n`,
  populated with entries from `entry-gen` (if supplied.)

  `entry-gen` defaults to [[ratio]]."
  ([n] (square-matrix n ratio))
  ([n entry-gen]
   (matrix n n entry-gen)))

;; ## Series

(defn series
  "Generates a [[series/Series]] instance of elements drawn from `entry-gen`.

  `entry-gen` defaults to [[gen/nat]]."
  ([] (series gen/nat))
  ([entry-gen]
   (gen/fmap ss/series* (gen/vector entry-gen))))

(defn power-series
  "Generates a [[series/PowerSeries]] instance of elements drawn from `entry-gen`.

  `entry-gen` defaults to [[gen/nat]]."
  ([] (power-series gen/nat))
  ([entry-gen]
   (gen/fmap ss/power-series* (gen/vector entry-gen))))

;; ## Vector Set
;;
;; These are used in the implementation of [[emmy.differential]].

(defn vector-set
  "Generates a sorted vector of distinct elements drawn from `entry-gen`.
  `entry-gen` must produce comparable elements.

  `entry-gen` defaults to [[gen/nat]]."
  ([] (vector-set gen/nat))
  ([entry-gen]
   (gen/fmap vs/make (gen/set entry-gen))))

;; ## Differential

(defn differential
  "Returns a generator that produces proper instances
  of [[emmy.differential/Differential]]."
  ([] (differential real))
  ([coeff-gen]
   (let [term-gen   (gen/let [tags (vector-set gen/nat)
                              coef coeff-gen]
                      (let [tags (if (empty? tags) [0] tags)
                            coef (if (g/zero? coef) 1 coef)]
                        (#'d/make-term tags coef)))]
     (gen/let [terms  (gen/vector term-gen 1 5)
               primal coeff-gen]
       (let [tangent-part (d/from-terms terms)
             ret          (d/d:+ primal tangent-part)]
         (cond (d/differential? ret)          ret
               (d/differential? tangent-part) tangent-part
               :else (d/from-terms [(#'d/make-term [] primal)
                                    (#'d/make-term [0] 1)])))))))

;; ## TapeCell

(defn tapecell
  "Returns a generator that produces instances of [[emmy.tape/TapeCell]] with tag
  of `tag` generated by repeated applications of binary and unary generic
  functions on primitive [[emmy.tape/TapeCell]] instances.

  These primitives use `primal-gen` to generate their primal parts."
  ([] (tapecell 0 real))
  ([primal-gen] (tapecell 0 primal-gen))
  ([tag primal-gen]
   (let [prim   (gen/let [primal primal-gen]
                  (tape/make tag primal))
         unary  (fn [inner]
                  (gen/let [x  inner
                            y  inner
                            op (gen/elements [g/* g/- g/+])]
                    (op x y)))
         binary (fn [inner]
                  (gen/let [x  inner
                            op (gen/elements [g/square g/cos g/sin g/tan])]
                    (op x)))]
     (recursive
      (fn [inner]
        (gen/let [flip gen/boolean]
          (if flip
            (unary inner)
            (binary inner))))
      prim))))

;; ## Polynomials

(defn poly:exponents [arity]
  (->> (gen/vector gen/nat arity)
       (gen/fmap xpt/dense->exponents)))

(defn poly:terms
  ([arity]
   (poly:terms arity small-integral))
  ([arity coeff-gen & opts]
   (let [expt-gen (poly:exponents arity)
         term-gen (gen/tuple expt-gen coeff-gen)]
     (apply gen/vector term-gen opts))))

(defn polynomial
  "Returns a generator that produces instances of [[polynomial.Polynomial]].

  `arity` can be a number or a generator."
  [& {:keys [arity coeffs nonzero?]
      :or {nonzero? true
           arity (gen/fmap inc gen/nat)
           coeffs small-integral}}]
  (letfn [(poly-gen [arity]
            (let [terms (poly:terms arity coeffs)
                  pgen (gen/fmap (fn [terms]
                                   (let [p (poly/make arity terms)]
                                     (if (poly/polynomial? p)
                                       p
                                       (poly/constant arity p))))
                                 terms)]
              (if nonzero?
                (gen/such-that (complement g/zero?) pgen)
                pgen)))]
    (let [arity (if (integer? arity)
                  (gen/return arity)
                  arity)]
      (gen/bind arity poly-gen))))

(defn rational-function
  "Returns a generator that produces instances
  of [[rational-function/RationalFunction]].

  `arity` can be a number or a generator."
  ([]
   (rational-function 1 {} {}))
  ([arity]
   (rational-function arity {} {}))
  ([arity num-opts denom-opts]
   (let [num-opts   (assoc num-opts
                           :arity arity)
         denom-opts (assoc denom-opts
                           :nonzero? true
                           :arity arity)
         n  (apply polynomial (apply concat num-opts))
         d  (apply polynomial (apply concat denom-opts))
         rf (gen/fmap
             (fn [[u v]]
               (rf/->reduced u v))
             (gen/tuple n d))]
     (gen/such-that rf/rational-function? rf))))

;; ## Custom Almost-Equality

(defn- eq-delegate
  "Takes a real number `this` on the left, and checks it for approximate equality
  with:

  - complex numbers by comparing to the real part and checking that `that`'s
    imaginary part is roughly ~0
  - real numbers with the default approximate check behavior
  - falls through to `=` for all other types.

  In CLJS, `this` can be `js/BigInt`, `google.math.Long`, `goog.math.Real`,
  `Fraction` or any of the other types in the numeric tower."
  [this that]
  (cond (c/complex? that) (and (si/*comparator* 0.0 (u/double (g/imag-part that)))
                               (si/*comparator*
                                (u/double this)
                                (u/double (g/real-part that))))
        (v/real? that)    (si/*comparator*
                           (u/double this)
                           (u/double that))
        :else             (v/= this that)))

(extend-protocol si/Approximate
  #?@(:cljs
      [Fraction
       (ish [this that] (eq-delegate this that))

       u/inttype
       (ish [this that] (eq-delegate this that))

       u/longtype
       (ish [this that] (eq-delegate this that))

       js/BigInt
       (ish [this that] (eq-delegate this that))

       number
       (ish [this that] (eq-delegate this that))])

  #?@(:clj
      [Double
       (ish [this that] (eq-delegate this that))

       Float
       (ish [this that] (eq-delegate this that))

       Number
       (ish [this that] (eq-delegate this that))])

  Complex
  (ish [this that]
    (cond (c/complex? that)
          (and (si/*comparator* (u/double (c/real this))
                                (u/double (c/real that)))
               (si/*comparator* (u/double (c/imaginary this))
                                (u/double (c/imaginary that))))

          (quat/quaternion? that)
          (si/ish that this)

          (v/real? that)
          (and (si/*comparator* 0.0 (u/double (c/imaginary this)))
               (si/*comparator*
                (u/double (c/real this)) (u/double that)))
          :else (v/= this that)))

  Quaternion
  (ish [this that]
    (cond (quat/quaternion? that)
          (and (si/*comparator* (u/double (quat/get-r this))
                                (u/double (quat/get-r that)))
               (si/*comparator* (u/double (quat/get-i this))
                                (u/double (quat/get-i that)))
               (si/*comparator* (u/double (quat/get-j this))
                                (u/double (quat/get-j that)))
               (si/*comparator* (u/double (quat/get-k this))
                                (u/double (quat/get-k that))))

          (c/complex? that)
          (and (si/*comparator* 0.0 (quat/get-j this))
               (si/*comparator* 0.0 (quat/get-k this))
               (si/*comparator* (u/double (quat/get-r this)) (c/real that))
               (si/*comparator* (u/double (quat/get-i this)) (c/imaginary that)))

          (v/real? that)
          (and (si/*comparator* 0.0 (u/double (quat/get-i this)))
               (si/*comparator* 0.0 (u/double (quat/get-j this)))
               (si/*comparator* 0.0 (u/double (quat/get-k this)))
               (si/*comparator*
                (u/double (quat/get-r this)) (u/double that)))

          :else (v/= this that)))

  Symbol
  (ish [this that]
    (if (symbol? that)
      (= this that)
      (v/= this that)))

  #?(:cljs s/Structure :clj Structure)
  (ish [this that]
    (cond (instance? #?(:cljs s/Structure :clj Structure) that)
          (and (s/same-orientation? this that)
               (si/ish (s/structure->vector this)
                       (s/structure->vector that)))

          (s/up? this)
          (cond (vector? that)  (si/ish (s/structure->vector this) that)
                (seqable? that) (si/ish (seq this) (seq that))
                :else false)
          :else false)))
