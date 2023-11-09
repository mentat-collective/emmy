#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.complex-test
  (:require #?(:cljs [cljs.reader :refer [read-string]])
            [clojure.test :refer [deftest is testing]]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.complex :as c]
            [emmy.complex.impl :as ci]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.generic-test :as gt]
            [emmy.laws :as l]
            [emmy.numbers]
            [emmy.value :as v]
            [same.core :refer [ish? with-comparator]]))

(defn ^:private near [w z]
  (< (g/abs (g/- w z)) 1e-10))

(defn ^:private near-enough
  "A relaxed form of near used when reciprocal operations can
  create some larger values which won't allow 10 matching figures
  after the decimal"
  [w z]
  (< (g/abs (g/- z w)) 1e-6))

(defn right-half-plane
  "If z is not in the right half plane, move it there by rotating it 180°
  (i.e., multiply by -1). This is used for inverse identity tests where the
  principal value of the inverse function is the right half-plane."
  [z]
  (if (g/negative? (c/real z))
    (g/negate z)
    z))

(deftest complex-literal
  (testing "complex constructor-as-parser can round-trip Complex instances. These
  show up as code snippets when you call `read-string` directly, and aren't evaluated
  into Clojure."
    (is (= `(c/complex [1 2])

           ;; string input:
           (read-string {:readers {'emmy/complex c/parse-complex}}
                        (pr-str #emmy/complex "1 + 2i"))

           ;; vector input:
           (read-string {:readers {'emmy/complex c/parse-complex}}
                        (pr-str #emmy/complex [1 2]))))

    (checking "complex constructor can handle strings OR direct inputs" 100
              [re (sg/reasonable-double)
               im (sg/reasonable-double)]
              (is (= (c/complex re im)
                     (c/complex (str re (if (< im 0) " - " " + ") (g/abs im) "i")))))

    (testing "complex inputs"
      (is (= (c/complex 1 2) #emmy/complex [1 2]))
      (is (= (c/complex 1.2) #emmy/complex 1.2))
      (is (= (c/complex 1.2) #emmy/complex 1.2))
      (is (= (c/complex 1.2 3.4)
             (c/complex "1.2+3.4i")
             #emmy/complex "1.2+3.4i")))))

(deftest complex-laws
  ;; Complex numbers form a field. We use a custom comparator to control some
  ;; precision loss.
  (with-comparator (v/within 1e-3)
    (l/field 100 sg/complex "Complex")))

(deftest generic-functions
  (testing "generic function implementation (formerly Value)"
    (is (every?
         g/zero?
         [(c/complex -0.0 -0.0)
          (c/complex 0.0 -0.0)
          (c/complex -0.0 0.0)
          (c/complex 0.0 0.0)
          (g/zero-like c/ONE)
          (g/zero-like (c/complex 100))
          c/ZERO
          #emmy/complex "0"])
        "negative zero doesn't affect zero")

    (is (not (g/zero? c/ONE)))
    (is (not (g/zero? (c/complex 1.0))))
    (is (= c/ZERO (g/zero-like (c/complex 2))))
    (is (= c/ZERO (g/zero-like #emmy/complex "0 + 3.14i")))

    (let [ones [c/ONE
                (c/complex 1.0)
                (g/one-like c/ZERO)
                (c/complex 1.0 0.0)
                (c/complex 1.0 -0.0)]]
      (is (every? g/one? ones)
          "-0 in imaginary does not affect one?")

      (is (every? g/identity? ones)
          "-0 in imaginary does not affect identity?"))

    (is (not (g/one? (c/complex 2))))
    (is (not (g/one? (c/complex 0.0))))

    (is (= 10 (g/freeze (c/complex 10)))
        "If the imaginary piece is 0, freeze will return only the real part.")
    (is (v/numerical? (c/complex 10)))

    (testing "exact?"
      (is (not (g/exact? (c/complex 0 10.1))))
      (is (g/exact? (c/complex 10)))
      (is (g/exact? (c/complex 10 12)))
      (is (not (g/exact? (c/complex 10.1))))
      (is (not (g/exact? (c/complex 10 12.1)))))))

(let [pi Math/PI]
  (deftest complex-numbers
    (testing "v/="
      (is (v/= #emmy/complex "1+0i" #emmy/bigint 1))
      (is (not (v/= #emmy/complex "1+2i" #emmy/ratio "1/2")))

      #?(:cljs
         (testing "CLJS can compare complex with non-complex using clojure.core/="
           (is (= #emmy/complex "1+0i" #emmy/bigint 1))
           (is (not= #emmy/complex "1+2i" #emmy/ratio "1/2")))))

    (testing "complex constructor and predicate"
      (is (c/complex? c/ONE))
      (is (c/complex? c/I))
      (is (c/complex? #emmy/complex "2"))
      (is (not (c/complex? 4))))

    (testing "complex-generics"
      (let [skip #{:quotient :gcd :remainder :modulo :negative? :exact-divide}]
        (gt/integral-tests c/complex :exclusions skip :eq near)
        (gt/floating-point-tests c/complex :eq near)))

    (checking "g/negative?, g/infinite?" 100 [x sg/real]
              (let [z (c/complex x 0)]
                (is (= (g/negative? x)
                       (g/negative? z))
                    "A complex number is negative if its imaginary component is
                    zero and real is negative, false otherwise."))

              (is (not
                   (g/negative?
                    (g/make-rectangular x 1)))
                  "Never negative if imaginary component is nonzero.")

              (is (not
                   (g/infinite?
                    (g/make-rectangular x x)))
                  "infinite? is never true for non-infinite inputs."))

    (testing "g/infinite?"
      (is (every?
           g/infinite?
           [(c/complex ##Inf ##Inf)
            (c/complex ##-Inf ##Inf)
            (c/complex ##Inf ##-Inf)
            (c/complex ##-Inf ##-Inf)
            (c/complex ##Inf 1)
            (c/complex 1 ##Inf)
            (c/complex ##-Inf 1)
            (c/complex 1 ##-Inf)])
          "an infinite or negative infinite value in either slot makes the
          complex number `g/infinite?`"))

    (testing "add"
      (is (= #emmy/complex "4 + 6i"
             (g/add #emmy/complex "1 + 2i"
                    #emmy/complex "3 + 4i")))
      (is (= (c/complex 1 3) (g/add (c/complex 0 3) 1)))
      (is (= (c/complex 1 3)
             (g/add 1 (c/complex 0 3))
             (g/add (c/complex 0 3) 1))))

    (testing "sub"
      (is (= (c/complex -2 -2) (g/sub (c/complex 1 2)
                                      (c/complex 3 4))))
      (is (= (c/complex 10 2) (g/sub (c/complex 20 2) 10)))
      (is (= (g/negate (c/complex 10 2))
             (g/sub 10 (c/complex 20 2)))))

    (testing "mul between numbers and complex numbers in both orders"
      ;; rotate 7 by pi/2
      (is (near (g/mul c/I 7) (g/mul 7 (g/exp (g/mul c/I (/ pi 2))))))
      (is (near (c/complex 0 7) (g/mul (c/complex 7) (g/exp (g/mul c/I (/ pi 2)))))))

    (testing "div in either order"
      (is (= (c/complex 0 -1) (g/div 1 c/I)))
      (is (= (c/complex 2 2) (g/div (c/complex 4 4) 2))))

    (testing "modulo examples"
      ;; https://stackoverflow.com/questions/54553489/how-to-calculate-a-modulo-of-complex-numbers
      (is (= (c/complex 1 1)
             (g/modulo (c/complex 8 2) (c/complex 2 1))))
      (is (= (c/complex -1 -1)
             (g/modulo (g/- (c/complex 8 2)) (g/- (c/complex 2 1)))))

      ;; https://www.quora.com/How-do-I-find-Modulo-of-complex-numbers
      (is (near (c/complex (g/- 24 (* 8 pi)))
                (g/modulo 24 (c/complex 0 (* 2 pi)))))

      ;; https://pressbooks.howardcc.edu/jrip3/chapter/equivalence-classes-of-complex-numbers-modulo-a-natural-number/
      (is (= (c/complex 1 2) (g/modulo (c/complex 4 5) 3)))
      (is (= (c/complex 1 1) (g/modulo (c/complex 4 -5) 3)))
      (is (= (c/complex 2 2) (g/modulo (c/complex -4 5) 3)))
      (is (= (c/complex 2 1) (g/modulo (c/complex -4 -5) 3)))

      (is (= (c/complex -2 2)
             (g/modulo (c/complex 6 4) (c/complex 3 5)))))

    (checking "make-rectangular with 0 complex == identity" 100
              [x sg/real]
              (is (= x (g/make-rectangular x 0.0))
                  "inexact zero on JVM")

              (is (= x (g/make-rectangular x 0))
                  "exact zero"))

    (checking "make-polar with 0 radius or angle == radius" 100
              [x sg/real]
              (is (= x (g/make-polar x 0.0)))
              (is (= 0.0 (g/make-polar 0.0 x)))
              (is (= 0 (g/make-polar 0 x))))

    (checking "make-rectangular with 0 complex == identity" 100
              [x sg/real]
              (is (= x (g/make-rectangular x 0.0))))

    (testing "integer-part"
      (is (= (c/complex 1 2) (g/integer-part (c/complex 1 2))))
      (is (= (c/complex 1 2) (g/integer-part (c/complex 1.5 2.9))))
      (is (= (c/complex -1 -2) (g/integer-part (c/complex -1.5 -2.9))))
      (is (= -1 (g/integer-part (c/complex -1.5 0.9)))
          "imaginary part drops off if == zero"))

    (checking "integer-part pushes through to complex components" 100
              [x sg/complex]
              (is (= (g/integer-part x)
                     (g/make-rectangular
                      (g/integer-part (g/real-part x))
                      (g/integer-part (g/imag-part x))))))

    (testing "fractional-part unit tests"
      (is (near (c/complex 0.5 0.9) (g/fractional-part (c/complex 1.5 2.9))))
      (is (near (c/complex 0.5 0.1) (g/fractional-part (c/complex -1.5 -2.9))))
      (is (= 0 (g/fractional-part (c/complex 1 2))))
      (is (= 0.5 (g/fractional-part (c/complex -1.5 2)))
          "imaginary part drops off if == zero"))

    (checking "fractional-part pushes through to complex components" 100
              [x sg/complex]
              (is (= (g/fractional-part x)
                     (g/make-rectangular
                      (g/fractional-part (g/real-part x))
                      (g/fractional-part (g/imag-part x))))))

    (testing "floor"
      (is (= (c/complex 1 2) (g/floor (c/complex 1 2))))
      (is (= (c/complex 1 2) (g/floor (c/complex 1.5 2.9))))
      (is (= (c/complex -2 -3) (g/floor (c/complex -1.5 -2.9)))))

    (checking "floor pushes through to complex components" 100
              [x sg/complex]
              (is (v/= (g/floor x)
                     (g/make-rectangular
                      (g/floor (g/real-part x))
                      (g/floor (g/imag-part x))))))

    (testing "ceiling"
      (is (= (c/complex 1 2) (g/ceiling (c/complex 1 2))))
      (is (= (c/complex 2 3) (g/ceiling (c/complex 1.5 2.9))))
      (is (= (c/complex -1 -2) (g/ceiling (c/complex -1.5 -2.9)))))

    (checking "ceiling pushes through to complex components" 100
              [x sg/complex]
              (is (v/= (g/ceiling x)
                       (g/make-rectangular
                        (g/ceiling (g/real-part x))
                        (g/ceiling (g/imag-part x))))))

    (testing "expt"
      (is (near -1 (g/expt (c/complex 0 1) 2)))
      (is (near (c/complex 16) (g/expt 2 (c/complex 4))))
      (is (near (c/complex 16) (g/expt (c/complex 2) (c/complex 4))))

      (is (v/= -1 (g/square c/I))
          "squaring I produces an exact result.")

      (is (= c/-I (g/cube c/I))))

    (testing "negate"
      (is (= (c/complex -10 2)
             (g/negate (c/complex 10 -2)))))

    (testing "invert"
      (is (g/zero? (g/add c/I (g/invert c/I)))))

    (testing "abs"
      (is (= 5 (g/abs (c/complex 3 4)))))

    (testing "exp"
      (is (near (c/complex -1) (g/exp (g/mul c/I pi)))
          "Euler's identity"))

    (testing "log"
      (is (= (g/mul c/I pi) (g/log (g/exp (g/mul c/I pi))))))

    (testing "square"
      (is (near (g/mul c/I 200) (g/square (c/complex 10 10)))))

    (testing "cube"
      (is (near (c/complex 0 -8) (g/cube (g/* 2 c/I))))
      (is (near (c/complex 27) (g/cube (c/complex 3))))
      (is (near (c/complex -27) (g/cube (c/complex -3)))))

    (testing "sqrt/square are mutually inverse"
      ;; principal value for sqrt: right half-plane. If the value is in
      ;; the left half-plane, make it principal by rotating 180°.

      (checking "sqrt ∘ square = id" 100 [z (sg/reasonable-complex)]
                (is (near (right-half-plane z) (g/sqrt (g/square z)))))
      (checking "square ∘ sqrt = id" 100 [z (sg/reasonable-complex)]
                (is (near z (g/square (g/sqrt z))))))

    (testing "sqrt"
      (is (near (c/complex 10 10) (g/sqrt (g/mul c/I 200)))))

    (testing "arithmetic"
      (is (v/numerical? c/I)))))

(defn fourth-power-is-one?
  "Checks if x^4 is 1+0i. This is needed because the gcd-complex function returns
   solutions that are determined up to a factor of ±1 and ±i."
  [x]
  (let [x4 (g/expt x 4)
        re (g/real-part x4)
        im (g/imag-part x4)]
    (and (ish? re 1)
         (ish? im 0))))

(deftest gcd-tests
  (testing "gcd-complex"
    (checking "GCD of anything with itself is itself (up to sign)" 100
              [z sg/complex]
              (let [gaussian-z (c/round z)]
                (is (= (c/abs-real gaussian-z)
                       (g/gcd gaussian-z gaussian-z))))

              (is (= (c/abs-real z)
                     (g/gcd z z))
                  "also true for non-gaussians, only in this case!"))

    (checking "GCD of anything with 0 is itself, also for non-gaussian complex
              numbers (by definition)" 100 [z sg/complex]
              (is (= z (g/gcd z (c/complex 0 0))))
              (is (= z (g/gcd (c/complex 0 0) z))))

    (checking "GCD of anything with 1 is 1." 100 [z sg/complex]
              (is (fourth-power-is-one? (g/gcd (c/round z) (c/complex 1 0))))
              (is (fourth-power-is-one? (g/gcd (c/complex 1 0) (c/round z)))))

    (testing "gcd when args are equal"
      (is (= (c/complex 12.2 0)
             (g/gcd (c/complex 12.2 0) 12.2))
          "if args are v/= floating point input is okay")

      (is (= (c/complex 0 2)
             (g/gcd (c/complex 24 2) 12)
             (g/gcd 12 (c/complex 24 2)))
          "complex + real"))

    (letfn [(check [l r]
              (let [z (g/gcd l r)]
                (if (g/zero? z)
                  (is (and (g/zero? l)
                           (g/zero? r)))
                  (is (fourth-power-is-one?
                       (g/gcd (g// l z)
                              (g// r z)))))))]
      (checking "dividing out the GCD gives coprime results" 100
                [l sg/complex r sg/complex]
                (let [gaussian-l (c/round l)
                      gaussian-r (c/round r)
                      z (g/gcd gaussian-l gaussian-r)]
                  (when-not (or (g/zero? gaussian-l)
                                (g/zero? gaussian-r))
                    (is (not (neg? (g/real-part z)))
                        "real part of the GCD is always positive, unless either
                        side to gcd is 0."))

                  (check gaussian-l gaussian-r)
                  (check (g/real-part gaussian-l) gaussian-r)
                  (check gaussian-l (g/real-part gaussian-r)))))))

(deftest trig-tests
  ;; Mathematica:
  ;;   fns = {Sin, Cos, ..., ArcCoth}
  ;;   z = 0.1 + 0.2 I
  ;;   Table[{f, NumberForm[f[z], 12]}, {f, fns}]
  ;; The results of that expression were massaged into the
  ;; Clojure-readable form below.
  (testing "one particular value"
    (let [z (c/complex 0.1 0.2)
          expected {:Sin #emmy/complex "0.101836749421+0.200330161149I"
                    :Cos #emmy/complex "1.0149706707-0.0201000610277I"
                    :Tan #emmy/complex "0.0963881308565+0.19928415106I"
                    :Csc #emmy/complex "2.01645361897-3.96670632883I"
                    :Sec #emmy/complex "0.984863898536+0.0195038389147I"
                    :Cot #emmy/complex "1.9669102428-4.06662142386I"
                    :Sinh #emmy/complex "0.0981700839054+0.199663505514I"
                    :Cosh #emmy/complex "0.984970995703+0.0199000611944I"
                    :Tanh #emmy/complex "0.103721150028+0.200614484227I"
                    :Csch #emmy/complex "1.98311860447-4.03337143727I"
                    :Sech #emmy/complex "1.01484407288-0.0205036079653I"
                    :Coth #emmy/complex "2.03357864486-3.93328969902I"
                    :ArcSin #emmy/complex "0.0981975111335+0.19963938539I"
                    :ArcCos #emmy/complex "1.47259881566-0.19963938539I"
                    :ArcTan #emmy/complex "0.103748113218+0.200586618131I"
                    :ArcCsc #emmy/complex "0.453870209963-2.19857302792I"
                    :ArcSec #emmy/complex "1.11692611683+2.19857302792I"
                    :ArcCot #emmy/complex "1.46704821358-0.200586618131I"
                    :ArcSinh #emmy/complex "0.10186391598+0.200303571099I"
                    :ArcCosh #emmy/complex "0.19963938539+1.47259881566I"
                    :ArcTanh #emmy/complex "0.096415620203+0.199261222833I"
                    :ArcCsch #emmy/complex "2.18358521656-1.09692154883I"
                    :ArcSech #emmy/complex "2.19857302792-1.11692611683I"
                    :ArcCoth #emmy/complex "0.096415620203-1.37153510396I"}]
      (is (near (:Sin expected) (g/sin z)))
      (is (near (:Cos expected) (g/cos z)))
      (is (near (:Tan expected) (g/tan z)))
      (is (near (:Csc expected) (g/csc z)))
      (is (near (:Sec expected) (g/sec z)))
      (is (near (:Cot expected) (g/cot z)))
      (is (near (:Sinh expected) (g/sinh z)))
      (is (near (:Cosh expected) (g/cosh z)))
      (is (near (:Tanh expected) (g/tanh z)))
      (is (near (:Csch expected) (g/csch z)))
      (is (near (:Sech expected) (g/sech z)))
      (is (near (:Coth expected) (g/coth z)))
      (is (near (:ArcSin expected) (g/asin z)))
      (is (near (:ArcCos expected) (g/acos z)))
      (is (near (:ArcTan expected) (g/atan z)))
      (is (near (:ArcCsc expected) (g/acsc z)))
      (is (near (:ArcSec expected) (g/asec z)))
      (is (near (:ArcCot expected) (g/acot z)))
      (is (near (:ArcSinh expected) (g/asinh z)))
      (is (near (:ArcCosh expected) (g/acosh z)))
      (is (near (:ArcTanh expected) (g/atanh z)))
      (is (near (:ArcCsch expected) (g/acsch z)))
      (is (near (:ArcSech expected) (g/asech z)))
      (is (near (:ArcCoth expected) (g/acoth z)))))
  (testing "reciprocal identities"
    ;; we relax `near` a bit because taking reciprocals moves
    ;; some of the significance to the left of the decimal point
    (checking "1/sin = csc" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/sin z)) (g/csc z))))
    (checking "1/csc = sin" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/csc z)) (g/sin z))))
    (checking "1/cos = sec" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/cos z)) (g/sec z))))
    (checking "1/sec = cos" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/sec z)) (g/cos z))))
    (checking "1/tan = cot" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/tan z)) (g/cot z))))
    (checking "1/cot = tan" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/cot z)) (g/tan z))))

    (checking "1/sinh = csch" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/sinh z)) (g/csch z))))
    (checking "1/csch = sinh" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/csch z)) (g/sinh z))))
    (checking "1/cosh = sech" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/cosh z)) (g/sech z))))
    (checking "1/sech = cosh" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/sech z)) (g/cosh z))))
    (checking "1/tanh = coth" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/tanh z)) (g/coth z))))
    (checking "1/coth = tanh" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/invert (g/coth z)) (g/tanh z)))))
  (testing "inverse identities"
    (checking "asin ∘ sin = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/asin (g/sin z)))))
    (checking "sin ∘ asin = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/sin (g/asin z)))))
    (checking "acos ∘ cos = id" 100 [z (sg/reasonable-complex)]
              (is (near (right-half-plane z) (g/acos (g/cos z)))))
    (checking "cos ∘ acos = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/cos (g/acos z)))))
    (checking "atan ∘ tan = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/atan (g/tan z)))))
    (checking "tan ∘ atan = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/tan (g/atan z)))))
    (checking "acsc ∘ csc = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/acsc (g/csc z)))))
    (checking "csc ∘ acsc = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/csc (g/acsc z)))))
    (checking "asec ∘ sec = id" 100 [z (sg/reasonable-complex)]
              (is (near (right-half-plane z) (g/asec (g/sec z)))))
    (checking "sec ∘ asec = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/sec (g/asec z)))))
    (checking "acot ∘ cot = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/acot (g/cot z)))))
    (checking "cot ∘ acot = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/cot (g/acot z)))))

    (checking "asinh ∘ sinh = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/asinh (g/sinh z)))))
    (checking "sinh ∘ asinh = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/sinh (g/asinh z)))))
    (checking "acosh ∘ cosh = id" 100 [z (sg/reasonable-complex)]
              (is (near (right-half-plane z) (g/acosh (g/cosh z)))))
    (checking "cosh ∘ acosh = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/cosh (g/acosh z)))))
    (checking "atanh ∘ tanh = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/atanh (g/tanh z)))))
    (checking "tanh ∘ atanh = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/tanh (g/atanh z)))))
    (checking "acsch ∘ csch = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/acsch (g/csch z)))))
    (checking "csch ∘ acsch = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/csch (g/acsch z)))))
    (checking "asech ∘ sech = id" 100 [z (sg/reasonable-complex)]
              (is (near (right-half-plane z) (g/asech (g/sech z)))))
    (checking "sech ∘ asech = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/sech (g/asech z)))))
    (checking "acoth ∘ coth = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/acoth (g/coth z)))))
    (checking "coth ∘ acoth = id" 100 [z (sg/reasonable-complex)]
              (is (near z (g/coth (g/acoth z))))))

  (testing "Pythagorean identities"
    (checking "sin^2 + cos^2 = 1" 100 [z (sg/reasonable-complex)]
              (is (near 1 (g/+ (g/square (g/sin z)) (g/square (g/cos z))))))
    (checking "cosh^2 - sinh^2 = 1" 100 [z (sg/reasonable-complex)]
              (is (near 1 (g/- (g/square (g/cosh z)) (g/square (g/sinh z))))))
    (checking "1 - tanh^2 = sech^2" 100 [z (sg/reasonable-complex)]
              (is (near (g/- 1 (g/square (g/tanh z))) (g/square (g/sech z)))))
    (checking "1 - coth^2 = csch^2" 100 [z (sg/reasonable-complex)]
              (is (near-enough (g/- 1 (g/square (g/coth z)))
                               (g/negate (g/square (g/csch z)))))))

  (testing "parsing"
    (is (= (c/complex 2 0) (ci/parse "2")))
    (is (= (c/complex 2 0) (ci/parse "2+0i")))
    (is (= (c/complex 0 2) (ci/parse "0+2i")))
    (is (= (c/complex 1 -2) (ci/parse "1+-2i")))
    (is (= (c/complex 0 2) (ci/parse "0+2i")))
    (is (= (c/complex 0 -2) (ci/parse "0-2i")))
    (is (= (c/complex 0 2) (ci/parse "0--2i")))
    (is (= (c/complex 1 2) (ci/parse "1.0e0 + +2.0e0I")))
    (is (= (c/complex 1 -2) (ci/parse "1.0e0 + -2.0e0I")))
    (is (= (c/complex 1 -2) (ci/parse "1.0e0 - +2.0e0I")))
    (is (= (c/complex 1 2) (ci/parse "1.0e0 - -2.0e0I")))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (ci/parse "2i")))
    )
  (testing "some corner cases"
    (is (g/infinite? (g/atan c/I)))
    (is (g/infinite? (g/atan (g/negate c/I))))
    (is (g/infinite? (g/asec c/ZERO)))
    (is (g/infinite? (g/acsc c/ZERO)))
    (is (near (c/complex (g// Math/PI 2)) (g/acot c/ZERO)))
    (is (g/infinite? (g/asech c/ZERO)))
    (is (g/infinite? (g/acsch c/ZERO)))
    (is (ci/nan? (g// c/ZERO c/ZERO)))
    (is (ci/nan? (g// ci/INFINITY ci/INFINITY)))
    (is (g/infinite? (g// ci/INFINITY c/ZERO)))

    ;; In Emmy a zero in a `*` expression annihilates everything,
    ;; preventing this particular behavior from being observed.
    (is (ci/nan? (ci/mul c/ZERO ci/INFINITY)))
    (is (ci/nan? (ci/mul ci/INFINITY c/ZERO)))
    ;; In contrast:
    (is (g/zero? (g/* c/ZERO ci/INFINITY)))
    (is (g/zero? (g/* ci/INFINITY c/ZERO)))

    (is (g/infinite? (g/* c/I ci/INFINITY)))
    (is (g/infinite? (g/* ci/INFINITY c/I)))
    (is (g/zero? (g/expt c/ZERO c/ONE)))
    (is (ci/nan? (g/expt c/ZERO c/I)))
    (is (ci/equal? (c/complex 2 0) 2))
    (is (ci/nan? (c/complex ##NaN 0)))
    (is (ci/nan? (c/complex 0 ##NaN)))))


(deftest promotions-from-real
  (is (= (c/complex 0 1) (g/sqrt -1)))
  (is (near (c/complex 1.57079632679489 -0.443568254385115) (g/asin 1.1)))
  (is (near (c/complex 0 0.4435682543851153) (g/acos 1.1)))
  (is (near (c/complex 0 Math/PI) (g/log -1))))

(deftest extra-functions
  (testing "functions needed for docs"
    (is (near (g/real-part (c/complex 3 4)) 3))
    (is (near (g/imag-part (c/complex 3 4)) 4))
    (is (near (g/imag-part (g/conjugate (c/complex 3 4))) -4))
    (is (near (g/magnitude (c/complex 0 1)) 1))
    (is (near (g/magnitude (c/complex 1 0)) 1))
    (is (near (g/magnitude (c/complex 1 1)) (g/sqrt 2))))

  (checking "transpose, determinant act as id" 100 [z sg/complex]
            (is (= z (g/transpose z)))
            (is (= z (g/determinant z))))

  (checking "conjugate/magnitude" 100 [z sg/complex]
            (is (ish? (g/magnitude z)
                      (g/real-part
                       (g/sqrt
                        (g/* z (g/conjugate z)))))))

  (checking "real/imag-part" 100 [z sg/complex]
            (is (= (g/negate (g/imag-part z))
                   (g/imag-part (g/conjugate z))))

            (is (= (g/real-part z)
                   (g/real-part (g/conjugate z))))

            (is (= z (g/+ (g/real-part z)
                          (g/* #emmy/complex "0+1i"
                               (g/imag-part z))))))

  (checking "angle" 100 [z sg/complex]
            (is (near (g/angle z)
                      (g/atan
                       (g/imag-part z)
                       (g/real-part z))))
            (let [rt (g/* (g/magnitude z)
                          (g/exp (g/* #emmy/complex "0+1i"
                                      (g/angle z))))]
              (with-comparator (v/within 1e-8)
                (is (ish? (g/real-part z)
                          (g/real-part rt)))
                (is (ish? (g/imag-part z)
                          (g/imag-part rt))))))

  (checking "exp-1" 100 [z (sg/reasonable-complex)]
            (is (near (g/- (g/exp z) 1) (ci/exp-1 z)))))
