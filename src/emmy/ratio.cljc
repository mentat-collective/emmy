#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.ratio
  "This namespace provides a number of functions and constructors for working
  with ratios in Clojure and ClojureScript.

  [[clojure.lang.Ratio]] is native in Clojure. The ClojureScript implementation
  uses [Fraction.js](https://github.com/infusion/Fraction.js/).

  For other numeric extensions, see [[emmy.numbers]]
  and [[emmy.complex]]."
  (:refer-clojure :exclude [ratio? numerator denominator rationalize])
  (:require #?(:clj [clojure.core :as core])
            #?(:clj [clojure.edn] :cljs [cljs.reader])
            #?(:cljs [emmy.bigfraction :as bf])
            [emmy.generic :as g]
            [emmy.util :as u]
            [emmy.value :as v])
  #?(:clj (:import (clojure.lang Ratio))))

(def ^:no-doc ratiotype
  #?(:clj Ratio :cljs ::ratio))

(derive ratiotype ::v/real)

(def ratio?
  #?(:clj core/ratio?
     :cljs (fn [r] (instance? bf/Fraction r))))

(defprotocol IRational
  (numerator [_])
  (denominator [_]))

(extend-protocol IRational
  #?(:clj Object :cljs default)
  (numerator [x] x)
  (denominator [_] 1)

  #?@(:clj
      [Ratio
       (numerator [r] (core/numerator r))
       (denominator [r] (core/denominator r))]

      :cljs
      [bf/Fraction
       (numerator [x] (bf/numerator x))
       (denominator [x] (bf/denominator x))]))

(defn rationalize
  "Construct a ratio."
  ([x]
   #?(:cljs (cond (v/integral? x) x
                  (instance? bf/Fraction x) x
                  (v/real? x) (bf/real-> x)
                  :else (u/arithmetic-ex (str "Cannot rationalize " x)))
      :clj (core/rationalize x)))
  ([n d]
   #?(:cljs (bf/promote (bf/make n d))
      :clj (core/rationalize (/ n d)))))

(def ^:private ratio-pattern #"(-?\d+)/(\d+)")

(defn parse-ratio
  "Parser for the `#emmy/ratio` literal."
  [x]
  (cond #?@(:clj
            [(ratio? x)
             `(rationalize
               (u/bigint ~(str (numerator x)))
               (u/bigint ~(str (denominator x))))])

        (v/number? x) `(emmy.ratio/rationalize ~x)
        (string? x)    (if-let [[_ numerator denominator] (re-matches ratio-pattern x)]
                         `(rationalize (u/bigint ~numerator) (u/bigint ~denominator))
                         (u/illegal (str "Invalid ratio: " x)))
        (and (vector? x) (= 2 (count x))) `(rationalize ~@x)

        :else (u/illegal (str "Invalid ratio: " x))))

(defmethod g/exact? [ratiotype] [_] true)
(defmethod g/freeze [ratiotype] [x]
  (let [n (numerator x)
        d (denominator x)]
    (if (g/one? d)
      n
      `(~'/ ~(g/freeze n) ~(g/freeze d)))))

#?(:clj
   (extend-type Ratio
     v/Numerical
     (numerical? [_] true)

     v/IKind
     (kind [_] Ratio)))

#?(:clj
   (do
     (defmethod g/gcd [Ratio ::v/integral] [a b]
       (g/div (.gcd (core/numerator a)
                    (biginteger b))
              (core/denominator a)))

     (defmethod g/gcd [::v/integral Ratio] [a b]
       (g/div (.gcd (biginteger a)
                    (core/numerator b))
              (core/denominator b)))

     (defmethod g/gcd [Ratio Ratio] [a b]
       (g/div (.gcd (core/numerator a)
                    (core/numerator b))
              (g/lcm (core/denominator a)
                     (core/denominator b))))

     (defmethod g/infinite? [Ratio] [_] false)

     (doseq [[op f] [[g/exact-divide /]
                     [g/quotient quot]
                     [g/remainder rem]
                     [g/modulo mod]]]
       (defmethod op [Ratio Ratio] [a b] (f a b))
       (defmethod op [Ratio ::v/integral] [a b] (f a b))
       (defmethod op [::v/integral Ratio] [a b] (f a b))))

   :cljs
   (do
     (defn- pow [r m]
       (let [n (numerator r)
             d (denominator r)]
         (if (neg? m)
           (rationalize (g/expt d (g/negate m))
                        (g/expt n (g/negate m)))
           (rationalize (g/expt n m)
                        (g/expt d m)))))

     ;; The -equiv implementation handles equality with any number, so flip the
     ;; arguments around and invoke equiv.
     (defmethod v/= [::v/real ::ratio] [l r] (= r l))

     ;; Note that fraction arithmetic automatically downcasts integral results
     ;; to the underlying bigint type.
     (defmethod g/add [::ratio ::ratio] [a b] (bf/promote (bf/add a b)))
     (defmethod g/sub [::ratio ::ratio] [a b] (bf/promote (bf/sub a b)))
     (defmethod g/mul [::ratio ::ratio] [a b] (bf/promote (bf/mul a b)))
     (defmethod g/div [::ratio ::ratio] [a b] (bf/promote (bf/div a b)))
     (defmethod g/exact-divide [::ratio ::ratio] [a b] (bf/promote (bf/div a b)))

     ;; In principle, one should not find an integral fraction object in the wild
     ;; due to the automatic downcasting. In theory the following three could just
     ;; return false, but let's test for a while with some assert statements.
     ;;
     (defmethod g/zero? [::ratio] [c] (bf/zero? c))
     (defmethod g/one? [::ratio] [c] (bf/one? c))
     (defmethod g/identity? [::ratio] [c] (bf/one? c))
     (defmethod g/zero-like [::ratio] [_] 0)
     (defmethod g/one-like [::ratio] [_] 1)
     (defmethod g/identity-like [::ratio] [_] 1)

     (defmethod g/negate [::ratio] [a] (bf/promote (bf/neg a)))
     (defmethod g/negative? [::ratio] [a] (bf/neg? a))
     (defmethod g/infinite? [::ratio] [_] false)
     (defmethod g/invert [::ratio] [a] (bf/promote (bf/invert a)))
     (defmethod g/square [::ratio] [a] (bf/promote (bf/mul a a)))
     (defmethod g/cube [::ratio] [a] (bf/promote (bf/integer-power a 3)))
     (defmethod g/abs [::ratio] [a] (bf/promote (bf/abs a)))
     (defmethod g/magnitude [::ratio] [a] (bf/promote (bf/abs a)))

     (defmethod g/expt [::ratio ::v/integral] [a b] (bf/promote (bf/integer-power a b)))
     (defmethod g/expt [::ratio ::ratio] [a b]
       (bf/promote
        (if (g/one? (bf/denominator b))
          (bf/integer-power a (bf/numerator b))
          (g/expt (bf/->real a) (bf/->real b)))))

     (defmethod g/sqrt [::ratio] [a]
       (if (neg? a)
         (g/sqrt (bf/->real a))
         (g/div (g/sqrt (numerator a))
                (g/sqrt (denominator a)))))

     (defmethod g/quotient [::ratio ::ratio] [a b] (bf/promote (bf/quotient a b)))
     (defmethod g/remainder [::ratio ::ratio] [a b] (bf/promote (bf/remainder a b)))
     (defmethod g/modulo [::ratio ::ratio] [a b]
       (bf/promote
        (bf/remainder (bf/add (bf/remainder a b) b) b)))
     (defmethod g/gcd [::ratio ::ratio] [a b] (bf/promote (bf/gcd a b)))

;; Cross-compatibility with numbers in CLJS.
     (defn- downcast-fraction
       "Anything that `upcast-number` doesn't catch will hit this and pull a floating
        point value out of the ratio."
       [op]
       (defmethod op [::ratio ::v/real] [a b]
         (op (bf/->real a) b))

       (defmethod op [::v/real ::ratio] [a b]
         (op a (bf/->real b))))

     (defn- upcast-number
       "Integrals can stay exact, so they become ratios before op."
       [op]
       (defmethod op [::ratio ::v/integral] [a b]
         (op a (bf/integer-> b)))

       (defmethod op [::v/integral ::ratio] [a b]
         (op (bf/integer-> a) b)))

     ;; An exact number should become a ratio rather than erroring out, if one
     ;; side of the calculation is already rational (but not if neither side
     ;; is).
     (upcast-number g/exact-divide)
     (upcast-number g/gcd)

     ;; We handle the cases above where the exponent connects with integrals and
     ;; stays exact.
     (downcast-fraction g/expt)

     (doseq [op [g/add g/mul g/sub g/gcd g/lcm
                 g/modulo g/remainder
                 g/quotient g/div]]
       (upcast-number op)
       (downcast-fraction op))))
