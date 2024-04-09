#_"SPDX-License-Identifier: GPL-3.0"

;; /**
;;  * @license Complex.js v2.1.1 12/05/2020
;;  *
;;  * Copyright (c) 2020, Robert Eisele (robert@xarg.org)
;;  * Dual licensed under the MIT or GPL Version 2 licenses.
;;  **/

(ns emmy.complex.impl
  "This namespace provides the primitive implementation of complex
   arithmetic for Emmy. The authors are indebted to Robert Eisele's
   `Complex.js` implementation from which we have borrowed freely and
   which is licensed as follows:

   * Copyright (c) 2020, Robert Eisele (robert@xarg.org)
   * Dual licensed under the MIT or GPL Version 2 licenses."
  (:refer-clojure :exclude [abs zero?])
  (:require [emmy.generic :as g]
            [emmy.util :as u]
            [emmy.value :as v]))

(declare equal? ->string)

(deftype Complex [re im]
  v/IKind
  (kind [_] :emmy.complex/complex)

  v/INumericTower

  #?@(:clj [Object
            (equals [a b] (equal? a b))
            (toString [a] (->string a))]

      :cljs [Object
             (toString [a] (->string a))

             IEquiv
             (-equiv [a b] (equal? a b))

             IPrintWithWriter
             (-pr-writer
              [z writer _]
              (write-all writer (->string z)))]))

#?(:clj
   (defmethod print-method Complex [^Complex z ^java.io.Writer w]
     (.write w (->string z))))

(defn- ->string
  [^Complex c]
  (str "#emmy/complex " [(.-re c) (.-im c)]))

(def ZERO (Complex. 0 0))
(def ONE (Complex. 1 0))
(def I (Complex. 0 1))
(def -I (Complex. 0 -1))
(def INFINITY (Complex. ##Inf ##Inf))
(def NAN (Complex. ##NaN ##NaN))
(def LN2 (Math/log 2))

(def ^:private PI:2 (/ Math/PI 2))
(def ^:private PI:4 (/ Math/PI 4))

(defn equal?
  "Returns true if the supplied complex number `z` is equal to the value `w`. The
  rules for [[eq]] are as follows:

   - If `w` is complex, returns true if all coefficients match, false
     otherwise

   - If `w` is sequential with a count of 2, it's interpreted as complex in the
     obvious way

  Else, if `z` is a [[real?]] complex number, returns true if the real component of
  `z` is [[emmy.value/=]] to `w`, false otherwise."

  [^Complex z w]
  (let [r (.-re z)
        i (.-im z)]
    (cond (instance? Complex w)
          (and (v/= r (.-re w))
               (v/= i (.-im w)))

          (v/real? w)
          (and (g/zero? i)
               (v/= r w))

          ;; Defer to `v/=` to support quaternion, octonion equality etc.
          :else (v/= z w))))

(defn zero?
  "Determines whether or not a complex number is at the zero pole of the
  Riemann sphere."
  [^Complex z]
  (and (g/zero? (.-re z))
       (g/zero? (.-im z))))

(defn nan?
  "Determines whether a complex number is not on the Riemann sphere."
  [^Complex z]
  (or (u/nan? (.-re z))
      (u/nan? (.-im z))))

(def ^:no-doc cos-1-square-terms
  "For the origin of these constants, see the related material
   in [[emmy.series-test/cos-1-square-terms]]."
  [4.779477332387385E-14 -1.147074559772972E-11 2.08767569878681E-9 -2.755731922398589E-7
   2.48015873015873E-5 -0.001388888888888889 0.04166666666666667 -0.5 0.0])

(defn cos-1
  "Computes $\\cos(x)-1$ using Taylor series if $|x|\\le{\\pi\\over 4}$),
   otherwise just subtracts one from `(Math/cos x)`. Doing the latter
   when x is small squanders significant digits."
  [x]
  (cond (g/zero? x) (g/zero-like x)
        (> (g/abs x) PI:4) (- (Math/cos x) 1)
        :else
        (let [xx (* x x)]
          (reduce (fn [a b] (+ (* a xx) b)) cos-1-square-terms))))

(defn real
  [^Complex z]
  (.-re z))

(defn imaginary
  [^Complex z]
  (.-im z))

(defn abs
  "Calculate the magnitude of the complex number.
  The implementation tactic used here (pre-dividing by the greater
  of the components) comes from the GNU ISO C++ standard library."
  [^Complex z]
  (let [x (.-re z)
        y (.-im z)
        s (max (g/abs x) (g/abs y))]
    (if (g/zero? s) s
        (let [x (g// x s)
              y (g// y s)]
          (g/* s (g/sqrt (g/+ (g/square x) (g/square y))))))))

(def ^:private
  complex-re
  "Regular expression used to parse complex numbers"
  #"([+-]?\d+(\.\d*)?([Ee][+-]?\d+)?)(\s?([+-])?\s?([+-]?\d+(\.\d*)?([Ee][+-]?\d+)?)[Ii])?")

(defn parse
  "Parse a complex number. We expect one or two floating point numbers.
  If two, they must be separated by a sign (perhaps surrounded by at most
  one space, the second number followed by I or i. Example: 1.2-3.4i)"
  [s]
  (if-let [[_ re re-frac re-expt _ sign im im-frac im-expt] (re-matches complex-re s)]
    (->Complex ((if (or re-frac re-expt) u/parse-double u/parse-int) re)
               (* (if (= sign "-") -1 1)
                  (if im ((if (or im-frac im-expt) u/parse-double u/parse-int) im) 0)))
    (throw (ex-info "invalid complex number" {:input s}))))

(defn add
  "Compute the complex sum."
  [^Complex l ^Complex r]
  (->Complex (g/+ (.-re l) (.-re r))
             (g/+ (.-im l) (.-im r))))

(defn sub
  "Compute the complex difference."
  [^Complex l ^Complex r]
  (->Complex (g/- (.-re l) (.-re r))
             (g/- (.-im l) (.-im r))))

(defn mul
  "Compute the complex product."
  [^Complex l ^Complex r]
  (cond (or (and (g/infinite? l) (zero? r))
            (and (zero? l) (g/infinite? r)))
        NAN

        (or (g/infinite? l) (g/infinite? r))
        INFINITY  ;; NB: some libraries are more careful with the sign of infinity than this one

        :else (let [a (.-re l)
                    b (.-im l)
                    c (.-re r)
                    d (.-im r)]
                (if (and (g/zero? b) (g/zero? d))
                  (g/* a c)
                  (->Complex (g/- (g/* a c) (g/* b d))
                             (g/+ (g/* a d) (g/* b c)))))))

(defn div
  "Compute the complex quotient."
  [^Complex l ^Complex r]
  (cond (or (and (zero? l) (zero? r))
            (and (g/infinite? l) (g/infinite? r)))
        NAN

        (or (g/infinite? l) (zero? r))
        INFINITY

        (or (zero? l) (g/infinite? r))
        ZERO

        :else
        (let [a (.-re l)
              b (.-im l)
              c (.-re r)
              d (.-im r)]
          (cond (g/zero? d)
                (->Complex (g// a c) (g// b c))

                (< (g/abs c) (g/abs d))
                (let [x (g// c d)
                      t (g/+ (g/* c x) d)]
                  (->Complex (g// (g/+ (g/* a x) b) t)
                             (g// (g/- (g/* b x) a) t)))

                :else
                (let [x (g// d c)
                      t (g/+ (g/* d x) c)]
                  (->Complex (g// (g/+ a (g/* b x)) t)
                             (g// (g/- b (g/* a x)) t)))))))

(defn pow
  "Calculate the power of two complex numbers. 0 to any power is
   zero, unless that power has an imaginary component, in which case NaN.
   Integral powers of the imaginary unit are treated exactly; everything
   else uses logarithms."
  [^Complex l ^Complex r]
  (cond (g/zero? l) (if (g/zero? (.-im r)) l NAN)

        (and (= I l)
             (g/zero? (.-im r))
             (v/integral? (.-re r)))
        (nth [1 I -1 -I] (mod (.-re r) 4))

        :else (g/exp (g/* r (g/log l)))))

(defn sqrt
  "Calculate the complex square root"
  [^Complex z]
  (let [a (.-re z)
        age0 (>= a 0)
        b (.-im z)]
    (if (and (g/zero? b)
             age0)
      (->Complex (g/sqrt a) 0)
      (let [r (g/abs z)
            re (if age0
                 (g// (g/sqrt (g/* 2 (g/+ r a))) 2)
                 (g// (g/abs b) (g/sqrt (g/* 2 (g/- r a)))))
            im (if age0
                 (g// (g/abs b) (g/sqrt (g/* 2 (g/+ r a))))
                 (g// (g/sqrt (g/* 2 (g/- r a))) 2))]
        (->Complex re (if (g/negative? b) (g/negate im) im))))))

(defn exp
  "Calculate the complex exponential."
  [^Complex z]
  (let [ea (g/exp (.-re z))
        b (.-im z)]
    (->Complex (g/* ea (g/cos b)) (g/* ea (g/sin b)))))

(defn exp-1
  "Calculate the complex exponent and subtracts one.
  This may be more accurate than `(- (exp z) 1)` if
  `z` is small."
  [^Complex z]
  ;;   exp(a + i*b) - 1
  ;; = exp(a) * (cos(b) + j*sin(b)) - 1
  ;; = expm1(a)*cos(b) + cosm1(b) + j*exp(a)*sin(b)
  (let [a (.-re z)
        b (.-im z)]
    (->Complex (+ (* (Math/expm1 a) (Math/cos b)) (cos-1 b))
               (* (Math/exp a) (Math/sin b)))))

(defn arg
  "Calculate the angle of the complex number."
  [^Complex z]
  (g/atan (.-im z) (.-re z)))

(defn log
  "Calculate complex the natural log."
  [^Complex z]
  (->Complex (g/log (abs z)) (arg z)))

(defn sin
  "Calculate the complex sine."
  [^Complex z]
  ;; sin(z) = ( e^iz - e^-iz ) / 2i
  ;;        = sin(a)cosh(b) + i cos(a)sinh(b)
  (let [a (.-re z)
        b (.-im z)]
    (->Complex (g/* (g/sin a) (g/cosh b))
               (g/* (g/cos a) (g/sinh b)))))

(defn cos
  [^Complex z]
  ;; cos(z) = ( e^iz + e^-iz ) / 2
  ;;        = cos(a)cosh(b) - i sin(a)sinh(b)
  (let [a (.-re z)
        b (.-im z)]
    (->Complex (g/* (g/cos a) (g/cosh b))
               (g/* -1 (g/sin a) (g/sinh b)))))

(defn tan
  [^Complex z]
  ;; tan(z) = sin(z) / cos(z)
  ;;        = ( e^iz - e^-iz ) / ( i( e^iz + e^-iz ) )
  ;;        = ( e^2iz - 1 ) / i( e^2iz + 1 )
  ;;        = ( sin(2a) + i sinh(2b) ) / ( cos(2a) + cosh(2b) )
  (let [a (g/* 2 (.-re z))
        b (g/* 2 (.-im z))
        d (g/+ (g/cos a) (g/cosh b))]
    (->Complex (g// (g/sin a) d)
               (g// (g/sinh b) d))))

(defn cot
  [^Complex z]
  ;; cot(c) = i(e^(ci) + e^(-ci)) / (e^(ci) - e^(-ci))
  (let [a (g/* 2 (.-re z))
        b (g/* 2 (.-im z))
        d (g/- (g/cos a) (g/cosh b))]
    (->Complex (g// (g/negate (g/sin a)) d)
               (g// (g/sinh b) d))))

(defn sec
  [^Complex z]
  (let [a (.-re z)
        b (.-im z)
        d (g// (g/+ (g/cosh (g/* 2 b)) (g/cos (g/* 2 a))) 2)]
    ;; sec(c) = 2 / (e^(ci) + e^(-ci))
    (->Complex (g// (g/* (g/cos a) (g/cosh b)) d)
               (g// (g/* (g/sin a) (g/sinh b)) d))))

(defn csc
  [^Complex z]
  ;; csc(c) = 2i / (e^(ci) - e^(-ci))
  (let [a (.-re z)
        b (.-im z)
        d (g// (g/- (g/cosh (g/* 2 b)) (g/cos (g/* 2 a))) 2)]
    (->Complex (g// (g/* (g/sin a) (g/cosh b)) d)
               (g// (g/* -1 (g/cos a) (g/sinh b)) d))))

(defn asin
  "Calculate the complex arc sine"
  [^Complex z]
  (let [a (.-re z)
        b (.-im z)
        t1 (sqrt (->Complex (g/- (g/* b b) (g/* a a) -1)
                            (g/* -2 a b)))
        t2 (log (->Complex (g/- (.-re t1) b)
                           (g/+ (.-im t1) a)))]
    (->Complex (.-im t2) (g/negate (.-re t2)))))

(defn acos
  "Calculate the complex arc cosine"
  [^Complex z]
  ;; acos(c) = i * log(c - i * sqrt(1 - c^2))
  (let [a (.-re z)
        b (.-im z)
        t1 (sqrt (->Complex (g/- (g/* b b) (g/* a a) -1)
                            (g/* -2 a b)))
        t2 (log (->Complex (g/- (.-re t1) b)
                           (g/+ (.-im t1) a)))]
    (->Complex (- PI:2 (.-im t2))
               (.-re t2))))

(defn atan
  "Calculate the complex arctangent"
  [^Complex z]
  ;; atan(c) = i / 2 log((i + x) / (i - x))
  (let [a (.-re z)
        b (.-im z)]
    (cond (and (g/zero? a) (g/one? b))
          (->Complex 0 ##Inf)

          (and (g/zero? a) (g/one? (g/negate b)))
          (->Complex 0 ##-Inf)

          :else
          (let [d (g/+ (g/* a a)
                       (g/square (g/- 1 b)))
                t1 (log (->Complex (g// (g/- 1 (g/square b) (g/square a)) d)
                                   (g// (g/* -2 a) d)))]
            (->Complex (g// (.-im t1) -2)
                       (g// (.-re t1) 2))))))

(defn acot
  "Calculate the complex arc cotangent."
  [^Complex z]
  ;; acot(c) = i / 2 log((c - i) / (c + i))
  (let [a (.-re z)
        b (.-im z)]
    (if (g/zero? b)
      (->Complex (g/atan 1 a) 0)
      (let [d (g/+ (g/square a) (g/square b))]
        (if (g/zero? d)
          (atan (->Complex (if (g/zero? a) 0 (g// a 0))
                           (if (g/zero? b) 0 (g// (g/negate b) 0))))
          (atan (->Complex (g// a d)
                           (g// (g/negate b) d))))))))

(defn asec
  "Calculate the complex arc secant."
  [^Complex z]
  ;; asec(c) = -i * log(1 / c + sqrt(1 - i / c^2))
  (if (zero? z)
    (->Complex 0 ##Inf)
    (let [a (.-re z)
          b (.-im z)
          d (g/+ (g/square a) (g/square b))]
      ;; the JS source tested for d == 0, but that is ruled out by the
      ;; zero check above
      (acos (->Complex (g// a d)
                       (g// (g/negate b) d))))))

(defn acsc
  "Compute the complex arc cosecant."
  [^Complex z]
  ;; acsc(c) = -i * log(i / c + sqrt(1 - 1 / c^2))
  (if (zero? z)
    (->Complex PI:2 ##Inf)
    (let [a (.-re z)
          b (.-im z)
          d (g/+ (g/square a) (g/square b))]
      ;; original code had a check for d == 0 here, but that seems to
      ;; be ruled out by the case (zero? z) above
      (asin (->Complex (g// a d) (g// (g/negate b) d))))))

(defn sinh
  "Calculate the complex hyperbolic sine"
  [^Complex z]
  (let [a (.-re z)
        b (.-im z)]
    (->Complex (g/* (g/sinh a) (g/cos b))
               (g/* (g/cosh a) (g/sin b)))))

(defn cosh
  "Calculate the complex hyperbolic cosine"
  [^Complex z]
  (let [a (.-re z)
        b (.-im z)]
    (->Complex (g/* (g/cosh a) (g/cos b))
               (g/* (g/sinh a) (g/sin b)))))

(defn tanh
  "Caclulate the complex hyperbolic tangent"
  [^Complex z]
  (let [a (g/* 2 (.-re z))
        b (g/* 2 (.-im z))
        d (g/+ (g/cosh a) (g/cos b))]
    (->Complex (g// (g/sinh a) d)
               (g// (g/sin b) d))))

(defn coth
  "Calculate the complex hyperbolic cotangent"
  [^Complex z]
  ;; coth(c) = (e^c + e^-c) / (e^c - e^-c)
  (let [a (g/* 2 (.-re z))
        b (g/* 2 (.-im z))
        d (g/- (g/cosh a) (g/cos b))]
    (->Complex (g// (g/sinh a) d)
               (g// (g/negate (g/sin b)) d))))

(defn csch
  "Compute the complex hyperbolic cosecant."
  [^Complex z]
  ;; csch(c) = 2 / (e^c - e^-c)
  (let [a (.-re z)
        b (.-im z)
        d (g/- (g/cos (g/* 2 b)) (g/cosh (g/* 2 a)))]
    (->Complex (g// (g/* -2 (g/sinh a) (g/cos b)) d)
               (g// (g/* 2 (g/cosh a) (g/sin b)) d))))

(defn sech
  "Calculate the complex hyperbolic secant."
  [^Complex z]
  ;; sech(c) = 2 / (e^c + e^-c)
  (let [a (.-re z)
        b (.-im z)
        d (g/+ (g/cos (g/* 2 b)) (g/cosh (g/* 2 a)))]
    (->Complex (g// (g/* 2 (g/cosh a) (g/cos b)) d)
               (g// (g/* -2 (g/sinh a) (g/sin b)) d))))

(defn asinh
  "Calculate the arc hyperbolic sine."
  [^Complex z]
  ;; asinh(c) = log(c + sqrt(c^2 + 1))
  (let [t (asin (->Complex (.-im z) (g/negate (.-re z))))]
    (->Complex (g/negate (.-im t)) (.-re t))))

(defn acosh
  "Compute the complex arc hyperbolic cosine"
  [^Complex z]
  ;; acosh(c) = log(c + sqrt(c^2 - 1))
  (let [a (acos z)
        ra (.-re a)
        ia (.-im a)]
    (if (< ia 0)
      (->Complex (- ia) ra)
      (->Complex ia (- ra)))))

(defn atanh
  "Calculate the complex hyperbolic tangent."
  [^Complex z]
  ;; atanh(c) = log((1+c) / (1-c)) / 2
  (let [a (.-re z)
        b (.-im z)
        noIM (and (> a 1) (g/zero? b))
        om (g/- 1 a)
        op (g/+ 1 a)
        d (g/+ (g/square om) (g/square b))
        x (if (g/zero? d)
            (->Complex (if (== a -1) 0 (g// a 0))
                       (if (g/zero? b) 0 (g// b 0)))
            (->Complex (g// (g/- (g/* op om) (g/square b)) d)
                       (g// (g/+ (g/* b om) (g/* op b)) d)))]
    (->Complex (g// (g/log (abs x)) 2)
               (g// (arg x) (if noIM -2 2)))))

(defn acoth
  "Calculate the complex arc hyperbolic cotangent."
  [^Complex z]
  ;; acoth(c) = log((c+1) / (c-1)) / 2
  (if (zero? z)
    (->Complex 0 PI:2)
    (let [a (.-re z)
          b (.-im z)
          d (g/+ (g/square a) (g/square b))]
      ;; the JS source checks for d = 0 here but that is ruled out by the test above
      (atanh (->Complex (g// a d) (g// (g/negate b) d))))))

(defn acsch
  "Calculate the complex arc hyperbolic cosecant."
  [^Complex z]
  ;; acsch(c) = log((1+sqrt(1+c^2))/c)
  (let [a (.-re z)
        b (.-im z)]
    (if (g/zero? b)
      (if (g/zero? a)
        (->Complex ##Inf 0)
        (->Complex (g/log (g/+ a (g/sqrt (g/+ (g/square a) 1)))) 0))
      (let [d (g/+ (g/square a) (g/square b))]
        ;; the JS source treated the special case d == 0, but that can't
        ;; happen since d == 0 ==> b == 0, which is treated above
        (asinh (->Complex (g// a d)
                          (g// (g/negate b) d)))))))

(defn asech
  "Calculate the complex arc hyperbolic secant."
  [^Complex z]
  (if (zero? z)
    INFINITY
    (let [a (.-re z)
          b (.-im z)
          d (g/+ (g/square a) (g/square b))]
      ;; The JS source treated the special case d == 0, but that can't
      ;; happen because the case (zero? z) is treated above
      (acosh (->Complex (g// a d)
                        (g// (g/negate b) d))))))

(defn inverse
  "Calculate the complex inverse 1/z"
  [^Complex z]
  (cond (zero? z) INFINITY
        (g/infinite? z) ZERO
        :else (let [a (.-re z)
                    b (.-im z)
                    d (g/+ (g/* a a) (g/* b b))]
                (->Complex (g// a d)
                           (g// (g/negate b) d)))))

(defn conjugate
  "Returns the complex conjugate"
  [^Complex z]
  (->Complex (.-re z)
             (g/negate (.-im z))))

(defn neg
  "Gets the negated complex number"
  [^Complex z]
  (->Complex (g/negate (.-re z))
             (g/negate (.-im z))))

(defn ceil
  "Ceils the actual complex number"
  ([z] (ceil z 0))
  ([^Complex z places]
   (let [places (Math/pow 10 places)]
     (->Complex
      (-> (.-re z) (* places) (Math/ceil) (/ places))
      (-> (.-im z) (* places) (Math/ceil) (/ places))))))

(defn floor
  "Floors the actual complex number"
  ([z] (floor z 0))
  ([^Complex z places]
   (let [places (Math/pow 10 places)]
     (->Complex
      (-> (.-re z) (* places) (Math/floor) (/ places))
      (-> (.-im z) (* places) (Math/floor) (/ places))))))

(defn one-like
  "Constructs an identity-like complex number of the same kind as the example"
  [^Complex c]
  (->Complex (g/one-like (.-re c))
             (g/zero-like (.-im c))))

(defn zero-like
  "Constructs an identity-like complex number of the same kind as the example"
  [^Complex c]
  (->Complex (g/zero-like (.-re c))
             (g/zero-like (.-im c))))
