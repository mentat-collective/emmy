#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.bigfraction
  "A CLJS bigfraction is a coprime pair of JavaScript BigInts, with the
   sign carried in the numerator."
  (:refer-clojure :exclude [abs zero? neg?])
  (:require
   [clojure.core :as core]
   [goog.array :as garray]))

(def ^:private ^:const ZERO (js/BigInt 0))
(def ^:private ^:const ONE (js/BigInt 1))
(def ^:private ^:const TEN (js/BigInt 10))
(def ^:private ^:const -ONE (- ONE))

(defn- bigint?
  "Returns true if x is a BigInt. There is a similar function in [[emmy.util]],
   but we prefer that this library avoid that dependency."
  [x]
  (= "bigint" (goog/typeOf x)))

(declare eq cmp ->real)

(deftype Fraction [^js/BigInt n ^js/BigInt d]

  Object
  (valueOf [this] (->real this))
  (toString [_] (str n "/" d))

  IHash
  (-hash [x]
    (bit-xor
     (-hash (.-n x))
     (-hash (.-d x))))

  IComparable
  (-compare [this other]
    (cond (instance? Fraction other)
          (cmp this other)

          (bigint? other)
          (compare n (* d other))

          :else
          (let [o-value (.valueOf other)]
            (garray/defaultCompare this o-value))))

)

(def ^:private F_ONE (Fraction. ONE ONE))

(defn division-by-zero
  "Throws JS exception used to signal an attempt to construct a fraction
   with zero denominator."
  []
  (throw (js/Error "Fraction with zero denominator")))

(defn numerator [^Fraction x]
  (.-n x))

(defn denominator [^Fraction x]
  (.-d x))

(defn zero?
  "Returns true iff `x` is a zero fraction."
  [^Fraction x]
  (let [a (.-n x)]
    (== ZERO a)))

(defn one?
  "Returns true iff `x` is a unit fraction."
  [^Fraction x]
  (let [a (.-n x)
        b (.-d x)]
    (== a b)))

(defn eq [^Fraction x ^Fraction y]
  (let [a (.-n x)
        b (.-d x)
        c (.-n y)
        d (.-d y)]
    (and (== a c) (== b d))))

(defn bigint-gcd
  "GCD assuming a and b are BigInts > 0"
  [^js/BigInt a ^js/BigInt b]
  (loop [a a
         b b]
    (if (== b ZERO) a
        (recur b (js-mod a b)))))

(defn- bigint-abs [a]
  (if (< a 0) (* -ONE a) a))

(defn integer->
  "Create a fraction with unit denominator."
  [n]
  (->Fraction (js/BigInt n) ONE))

(defn ->real
  "Coerce a fraction to real by performing the division
   in the floating point domain"
  [^Fraction q]
  (/ (js/Number (.-n q)) (js/Number (.-d q))))

(defn ->normal-form
  "We assume we are given two BigInts, with b > 0. The GCD is divided out, and the
   sign is carried in the numerator."
  [^js/BigInt a ^js/BigInt b]
  (when (== ZERO b)
    (division-by-zero))
  (let [an (< a 0)
        a (bigint-abs a)
        bn (< b 0)
        b (bigint-abs b)
        g (bigint-gcd a b)
        neg (not= an bn)
        abs_c (/ a g)
        c (if neg (* -ONE abs_c) abs_c)
        d (/ b g)]
    (->Fraction c d)))

(defn make
  "Produces a fraction in canonical form. Note that the canonical form of an integer is
   an integer, so if `(one? b)` you just get a."
  [a b]
  (let [a (js/BigInt a)
        b (js/BigInt b)]
    (when (== 0 b)
      (division-by-zero))
    (->normal-form a b)))

(defn- js-expt
  "Use the `js*` escape clause to get access to the JavaScript `**` operator,
   which can exponentiate two BigInts exactly."
  [a b]
  (js* "(~{} ** ~{})" a b))

(def ^:private double-re #"(-?\d+)(\.(\d+))?([Ee]([+-]\d+))?")

(defn real->
  "Clojure converts the real to BigDecimal and rationalizes from that.
   The JVM documentation explains that the BigDecimal value will correspond
   to what would be printed for the double value. We attempt to do the
   same thing here by converting to a string and converting from there."
  [x]
  (let [s (.toString x)]
    (if-let [[_ int _ frac _ exp] (re-matches double-re s)]
      (let [scale (- (js/parseInt (or exp "0"))
                     (count (or frac "")))
            scale-neg (< scale 0)
            mantissa (js/BigInt (str int frac))
            exponent (js/BigInt (js-expt TEN (js/BigInt (Math/abs scale))))]
        (if scale-neg
          (->normal-form mantissa exponent)
          (Fraction. (* mantissa exponent) ONE)))
      (throw (js/Error (str "Cannot convert " x " to ratio."))))))

(defn abs
  "Absolute value of the fraction `x`."
  [^Fraction x]
  (let [n (.-n x)
        d (.-d x)]
    (if (< n 0) (->Fraction (* -ONE n) d) x)))

(defn neg
  "Negation of the fraction `x`."
  [^Fraction x]
  (let [n (.-n x)
        d (.-d x)]
    (->Fraction (* -ONE n) d)))

(defn neg?
  "True if $x<0$."
  [^Fraction x]
  (< (.-n x) 0))

(defn add
  "Returns the sum of `x` and `y`."
  [^Fraction x ^Fraction y]
  (let [a (.-n x)
        b (.-d x)
        c (.-n y)
        d (.-d y)]
    (->normal-form (+ (* a d) (* b c)) (* b d))))

(defn sub
  "Returns the difference of `x` and `y`."
  [^Fraction x ^Fraction y]
  (let [a (.-n x)
        b (.-d x)
        c (.-n y)
        d (.-d y)]
    (->normal-form (core/- (core/* a d) (core/* b c)) (core/* b d))))

(defn mul
  "Returns the product of `x` and `y`."
  [^Fraction x ^Fraction y]
  (let [a (.-n x)
        b (.-d x)
        c (.-n y)
        d (.-d y)]
    (->normal-form (core/* a c) (core/* b d))))

(defn div
  "Returns the quotient of `x` and `y`."
  [^Fraction x ^Fraction y]
  (let [a (.-n x)
        b (.-d x)
        c (.-n y)
        d (.-d y)]
    (->normal-form (core/* a d) (core/* b c))))

(defn invert
  "Returns the reciprocal of `x`, but throws if $x=0$."
  [^Fraction x]
  (let [a (.-n x)
        b (.-d x)
        neg (< a 0)]
    (when (== ZERO a)
      (division-by-zero))
    (if neg
      (->Fraction (* -ONE b) (* -ONE a))
      (->Fraction b a))))

(defn cmp
  "Compares the fractions `x` and `y`, returning -1, 0, or 1."
  [^Fraction x ^Fraction y]
  (let [a (.-n x)
        b (.-d x)
        c (.-n y)
        d (.-d y)
        s (- (* a d) (* b c))]
    (cond (< s 0) -1
          (> s 0) 1
          :else 0)))

(defn integer-power
  "Raises the fraction `x` to the integer power `n`."
  [^Fraction x n]
  (let [a (.-n x)
        b (.-d x)
        N (js/BigInt n)]
    (cond
      (= n ZERO) F_ONE
      (= n ONE) x
      (> N ZERO) (->normal-form (js-expt a N) (js-expt b N))
      :else (->normal-form (js-expt b (- N)) (js-expt a (- N))))))

(defn promote
  "If the fraction has a unit denominator, return the numerator, else the fraction."
  [^Fraction x]
  (if (== ONE (.-d x)) (.-n x) x))

(defn ceil
  "Ceiling. Result is a BigInt."
  [^Fraction x]
  (let [a (.-n x)
        b (.-d x)]
    (+ (/ a b) (if (or (< a 0)
                       (== (js-mod a b) ZERO))
                 ZERO
                 ONE))))

(defn floor
  "Floor. Result is a BigInt."
  [^Fraction x]
  (let [a (.-n x)
        b (.-d x)]
    (- (/ a b) (if (or (> a 0)
                       (== (js-mod a b) ZERO))
                 ZERO
                 ONE))))

(defn quotient
  "Fractions form a field, so somewhat dubiously the function returns
   the largest integer N for which $Ny\\le x$."
  [x y]
  (let [z (div x y)]
    (if (neg? z)
      (ceil z)
      (floor z))))

(defn remainder
  "If $q$ is `(quotient x y)`, returns $x-qy$."
  [x y]
  (sub x (mul (Fraction. (quotient x y) ONE) y)))

(defn gcd
  [^Fraction x ^Fraction y]
  (let [a (.-n x)
        b (.-d x)
        c (.-n y)
        d (.-d y)]
    (abs (->normal-form (* (bigint-gcd a c) (bigint-gcd b d)) (* b d)))))
