#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.tape-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.calculus.derivative :refer [D]]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.numerical.derivative :refer [D-numeric]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :as s]
            [emmy.tape :as t]
            #?(:cljs [emmy.util :as u])
            [emmy.value :as v]
            [same.core :refer [ish? with-comparator]]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest tapecell-type-tests
  (checking "tape? works" 100 [t (sg/tapecell gen/symbol)]
            (is (t/tape? t)))

  (defmethod g/zero? [#?(:clj String :cljs js/String)] [_] false)

  (testing "v/numerical? is always false"
    (checking "tapecell with numerical primal is STILL not numerical, so we stay
               excluded from numerical simplifications." 100
              [t (sg/tapecell sg/real)]
              (is (not (v/numerical? t))))

    (is (not (v/numerical? (t/make 0 "face")))
        "tapecell with non-numerical primal is not numerical"))

  (checking "native comparison operators work with tapecell" 100
            [l sg/real, r sg/real]
            (is (= (v/compare l r)
                   (v/compare (t/make 0 l) r)
                   (v/compare l (t/make 0 r)))))

  #?(:cljs
     (testing "comparison unit tests"
       (is (v/= 0 (t/make 0 0)))
       (is (v/= (u/bigint 0) (t/make 0 0)))
       (is (v/= (u/long 0) (t/make 0 0)))
       (is (v/= (u/int 0) (t/make 0 0)))))

  #?(:cljs
     ;; NOTE: JVM Clojure won't allow non-numbers on either side of < and
     ;; friends. Once we implement `v/<` we can duplicate this test for those
     ;; overridden versions, which should piggyback on compare.
     (let [real-minus-rationals (gen/one-of [sg/any-integral (sg/reasonable-double)])]
       (checking "[[TapeCell]] is transparent to native comparison operators" 100
                 [[l-num r-num] (gen/vector real-minus-rationals 2)]
                 (let [compare-bit (v/compare l-num r-num)]
                   (doseq [l [l-num (t/make 0 l-num)]
                           r [r-num (t/make 0 r-num)]]
                     (cond (neg? compare-bit)  (is (< l r))
                           ;; NOTE that we have v/= in the middle here,
                           ;; because [[emmy.tape/TapeCell]] instances use
                           ;; instance equality for `=`, but value equality for
                           ;; `v/=`.
                           (zero? compare-bit) (is (and (<= l r) (v/= l r) (>= l r)))
                           :else (is (> l r))))))))

  (testing "value protocol implementation"
    (let [zero (t/make 0 0)
          dx (t/make 0 0 {(t/make 0 'x) 1})]
      (is (g/zero? zero)
          "zero? returns true for an empty term list")

      (is (g/zero? dx)
          "returns true even if we have partials recorded")

      (is (v/= dx 0)
          "subtly, `dx` IS in fact equal to zero (using v/=); this can be used
      for control flow.")

      (is (not (= dx 0))
          "but clojure.core/= equality still returns 0.")

      (testing "g/one? responds true to a one primal"
        (is (g/one? (t/make 0 1)))
        (is (g/one? (t/make 0 1 {(t/make 0 'x) 1}))))

      (testing "g/identity? responds true to an `identity` primal"
        (is (g/identity? (t/make 0 1)))
        (is (g/identity? (t/make 0 1 {(t/make 0 'x) 1}))))

      (checking "*-like works" 100 [t (sg/tapecell)]
                (is (g/zero? (g/zero-like t)))
                (is (g/one? (g/one-like t)))
                (is (g/identity? (g/identity-like t))))

      (testing "equality, comparison"
        (checking "g/negative?, g/infinite?" 100 [x sg/real]
                  (let [elem (t/make 0 x)]
                    (is (= (g/negative? x)
                           (g/negative? elem))
                        "negative? operates on finite-part only.")

                    (is (not (g/infinite? elem))
                        "infinite? is always false for real finite parts.")))

        (testing "g/infinite?"
          (is (not (g/infinite? (t/make 0 10 {(t/make 0 1) ##Inf})))
              "g/infinite? only looks at the finite part right now. Not sure how
              we would get into an infinite derivative with non-infinite finite
              part, but marking this test here as documentation.")

          (is (every?
               g/infinite?
               [(t/make 0 ##-Inf)
                (t/make 0 ##Inf)])
              "an infinite or negative infinite value in the finite part slot
               makes the differential `g/infinite?`"))

        (checking "=, equiv ignore tangent parts" 100
                  [n sg/real-without-ratio]
                  (is (v/= (t/make 0 n) n)
                      "tapecell on the left works.")

                  (is (v/= n (t/make 0 n))
                      "tapecell on the right works.")

                  (is (t/equiv (t/make 0 n) n n (t/make 0 n) n)
                      "t/equiv matches = behavior, varargs"))

        (checking "eq uses tangent parts" 100 [n sg/real]
                  (is (t/eq (t/make 0 n) n n (t/make 0 n) n)
                      "eq is true with no tangent, varargs")

                  (is (not (t/eq (t/make 0 n {(t/make 0 'x) 1}) n))
                      "eq is FALSE with a tangent, bundle on left")

                  (is (not (t/eq n (t/make 0 n {(t/make 0 'x) 1})))
                      "eq is FALSE with a tangent, bundle on right")

                  (is (t/eq (t/make 0 n) (t/make 0 n))
                      "t/eq handles equality")

                  (is (not (t/eq (t/make 0 n) (t/make 1 n)))
                      "t/eq is false for [[Differential]]s with diff tags"))

        (checking "compare ignores tangent parts" 100
                  [l sg/real, r sg/real]
                  (is (= (v/compare l r)
                         (v/compare (t/make 0 l {(t/make 0 'x) 1}) r)
                         (v/compare l (t/make 0 r {(t/make 0 'x) 1})))
                      "differential works on either side.")

                  (is (= (t/compare l r)
                         (t/compare (t/make 0 l {(t/make 0 'x) r}) r)
                         (t/compare l (t/make 0 r {(t/make 0 'x) l})))
                      "t/compare can handle non-differential on either side, also
                    ignores tangents.")

                  (testing "{d,v}/compare l, l matches equals behavior, ignores tangents"
                    (let [l+dr (t/make 0 l {(t/make 0 'x) r})]
                      (is (zero? (t/compare l+dr l)))
                      (is (zero? (t/compare l l+dr)))
                      (is (zero? (v/compare l+dr l)))
                      (is (zero? (v/compare l l+dr))))))

        (testing "freeze, simplify, str"
          (let [not-simple (g/square
                            (g/square (t/make 0 'x {(t/make 0 'y) 1})))]
            (is (= '[TapeCell
                     0
                     (expt x 4)
                     [[[TapeCell 0 (expt x 2)
                        [[[TapeCell 0 x [[[TapeCell 0 y []] 1]]] (* 2 x)]]]
                       (* 2 (expt x 2))]]]
                   (g/freeze not-simple))
                "A frozen differential freezes each entry")

            (checking "simplify acts as identity" 100
                      [t (sg/tapecell gen/symbol)]
                      (is (identical? t (g/simplify t))))))))))

(deftest tape-api-tests
  (testing "tag-of"
    (checking "tag-of matches tape-tag for cells" 100 [tag gen/nat]
              (let [cell (t/make tag 1)]
                (is (= (t/tag-of cell)
                       (t/tape-tag cell))
                    "for tape cells, these should match")))

    (checking "for any other type tag == ##-Inf" 100 [x gen/any]
              (is (= ##-Inf (t/tag-of x))
                  "for tape cells, these should match")))

  (checking "deep-primal returns nested primal" 100 [p gen/any-equatable]
            (let [cell (t/make 0 (t/make 1 p))]
              (is (= p (t/deep-primal cell))
                  "for tape cells, these should match"))))

(deftest reverse-mode-tests
  (testing "topological-sort"
    (let [x (t/make 0 'x)
          y (t/make 0 'y)
          a (g/mul x y)
          b (g/sin x)
          z (g/add a b)]
      (is (= [z b a y x]
             (t/topological-sort z))
          "topological-sort returns the graph in reverse order."))))

(deftest lifted-fn-tests
  (letfn [(breaks? [f x]
            (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                         ((t/gradient f) x))))]
    (checking "integer-discontinuous derivatives work" 100 [x sg/real]
              (if (v/integral? x)
                (do (breaks? g/floor x)
                    (breaks? g/ceiling x)
                    (breaks? g/integer-part x)
                    (breaks? g/fractional-part x))

                (do (is (zero? ((t/gradient g/floor) x)))
                    (is (zero? ((t/gradient g/ceiling) x)))
                    (is (zero? ((t/gradient g/integer-part) x)))
                    (is (g/one? ((t/gradient g/fractional-part) x)))))))

  (testing "lift-n"
    (let [*   (t/lift-n g/* (fn [_] 1) (fn [_ y] y) (fn [x _] x))
          Df7 (t/gradient
               (fn x**7 [x] (* x x x x x x x)))
          Df1 (t/gradient *)
          Df0 (t/gradient (fn [_] (*)))]
      (is (v/= '(* 7 (expt x 6))
               (g/simplify (Df7 'x)))
          "functions created with lift-n can take many args (they reduce via the
          binary case!)")

      (is (= 1 (Df1 'x))
          "single-arity acts as id.")

      (is (zero? (Df0 'x))
          "zero-arity acts as constant")))

  (testing "exercise some of the lifted fns by comparing them to numeric
  derivatives."
    (let [f (fn [x]
              (g/+ (g/* (g/cos x) (g/sin x))
                   (g/+ (g/sin x) (g/expt x 2))
                   (g/+ (g/sin x) x)
                   (g/log (g/abs x))))
          Df         (t/gradient f)
          Df-numeric (D-numeric f)]
      (with-comparator (v/within 1e-6)
        (checking "exercise some lifted fns" 100
                  [n (gen/double*
                      {:infinite? false
                       :NaN? false
                       :min 1
                       :max 100})]
                  (is (ish? (Df-numeric n)
                            (Df n))
                      "Does numeric match autodiff?"))))))

(deftest sinc-etc-tests
  (is (zero? ((t/gradient g/sinc) 0)))
  (is (zero? ((t/gradient g/tanc) 0)))
  (is (zero? ((t/gradient g/sinhc) 0)))
  (is (zero? ((t/gradient g/tanhc) 0)))

  (letfn [(gen-double [min max]
            (gen/double*
             {:infinite? false
              :NaN? false
              :min min
              :max max}))]
    (with-comparator (v/within 1e-4)
      (checking "sinc" 100 [n (gen-double 1 50)]
                (is (ish? ((D-numeric g/sinc) n)
                          ((t/gradient g/sinc) n))))

      ;; attempting to limit to a region where we avoid the infinities at
      ;; multiples of pi/2 (other than 0).
      (checking "tanc" 100 [n (gen-double 0.01 (- (/ Math/PI 2) 0.01))]
                (is (ish? ((D-numeric g/tanc) n)
                          ((t/gradient g/tanc) n))))

      (checking "tanhc" 100 [n (gen-double 1 50)]
                (is (ish? ((D-numeric g/tanhc) n)
                          ((t/gradient g/tanhc) n)))))

    (with-comparator (v/within 1e-4)
      (checking "sinhc" 100 [n (gen-double 1 10)]
                (is (ish? ((D-numeric g/sinhc) n)
                          ((t/gradient g/sinhc) n)))))

    (with-comparator (v/within 1e-8)
      (checking "acot" 100 [n (gen-double 0.01 (- (/ Math/PI 2) 0.01))]
                (is (ish? ((D-numeric g/acot) n)
                          ((t/gradient g/acot) n))))

      (checking "asec" 100 [n (gen-double 3 100)]
                (is (ish? ((D-numeric g/asec) n)
                          ((t/gradient g/asec) n))))

      (checking "acsc" 100 [n (gen-double 3 100)]
                (is (ish? ((D-numeric g/acsc) n)
                          ((t/gradient g/acsc) n))))

      (checking "sech" 100 [n (gen-double 3 100)]
                (is (ish? ((D-numeric g/sech) n)
                          ((t/gradient g/sech) n))))

      (checking "coth" 100 [n (gen-double 1 3)]
                (is (ish? ((D-numeric g/coth) n)
                          ((t/gradient g/coth) n))))

      (checking "csch" 100 [n (gen-double 0.5 10)]
                (is (ish? ((D-numeric g/csch) n)
                          ((t/gradient g/csch) n))))

      (checking "acosh" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/acosh) n)
                          ((t/gradient g/acosh) n))))

      (checking "asinh" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/asinh) n)
                          ((t/gradient g/asinh) n))))

      (checking "atanh" 100 [n (gen-double 0.1 0.9)]
                (is (ish? ((D-numeric g/atanh) n)
                          ((t/gradient g/atanh) n))))

      (checking "acoth" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/acoth) n)
                          ((t/gradient g/acoth) n))))

      (checking "asech" 100 [n (gen-double 0.1 0.9)]
                (is (ish? ((D-numeric g/asech) n)
                          ((t/gradient g/asech) n))))

      (checking "acsch" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/acsch) n)
                          ((t/gradient g/acsch) n)))))))

(deftest basic-gradient-tests
  (is (= 0 ((t/gradient (fn [] 100))))
      "gradient of no-arg returns zero")

  (testing "gradient of linear returns slope"
    (is (= 2 ((t/gradient #(g/* 2 %)) 1)))
    (is (= 2 ((t/gradient #(g/* 2 %)) 'w))))

  (testing "square, cube"
    (is (= (g/* 2 'z) ((t/gradient g/square) 'z)))
    (is (= (g/* 3 (g/expt 'z 2)) ((t/gradient g/cube) 'z)))
    (is (= (g/* 3 (g/expt 'y 2))
           ((t/gradient #(g/expt % 3)) 'y))))

  (is (= (g// 1 (g/expt (g/cos 'x) 2))
         ((t/gradient g/tan) 'x)))

  (testing "gradient of a fn returning a structure returns the componentwise
            derivative"
    (is (= (s/up 2 (g/+ 't 't))
           ((t/gradient #(s/up (g/* 2 %) (g/* % %))) 't)))

    (is (= (s/up (g/- (g/sin 't)) (g/cos 't))
           ((t/gradient #(s/up (g/cos %) (g/sin %))) 't))))

  (testing "trig derivatives"
    (is (= '(/ 1 (sqrt (+ (* -1 (expt x 2)) 1)))
           (g/freeze
            (g/simplify ((t/gradient g/asin) 'x)))))

    (is (= '(/ -1 (sqrt (+ (* -1 (expt x 2)) 1)))
           (g/freeze
            (g/simplify ((t/gradient g/acos) 'x))))))

  (testing "log"
    (is (= '(/ 1 x)
           (g/freeze
            (g/simplify ((t/gradient g/log) 'x))))))

  (testing "chain rule"
    (is (= (g/* (g/cos (g/* 2 'u)) 2)
           ((t/gradient #(g/sin (g/* 2 %))) 'u)))

    (let [s g/sqrt
          u (fn [t] (g/expt (g/- (g/* 3 (s t)) 1) (g// 2 3)))
          y (fn [t] (g// (g/+ (u t) 2) (g/- (u t) 1)))]
      (is (ish? (/ -1 18)
                ((t/gradient y) 9)))))

  (testing "structural-functions"
    (is (= '(up (cos t) (* -1 (sin t)))
           (g/freeze
            (g/simplify ((t/gradient (s/up g/sin g/cos)) 't))))))

  (testing "structure / x works"
    (letfn [(f [x]
              (g// (s/up 1 2 3) x))]
      (is (= '(up (/ -1 (expt x 2))
                  (/ -2 (expt x 2))
                  (/ -3 (expt x 2)))
             (g/freeze
              (g/simplify
               ((t/gradient f) 'x))))))))

(deftest gradient-tests
  (testing "vector input, scalar output"
    (let [f (fn [[x y z]]
              (g/+ (g/expt x 4) (g/* x y z (g/cos x))))]
      (is (= '(down
               (+ (* -1 x y z (sin x))
                  (* 4 (expt x 3))
                  (* y z (cos x)))
               (* x z (cos x))
               (* x y (cos x)))
             (g/freeze
              (g/simplify
               ((t/gradient f) ['x 'y 'z])))))

      (is (= (g/simplify
              ((D f) ['x 'y 'z]))
             (g/simplify
              ((t/gradient f) ['x 'y 'z])))
          "reverse-mode matches forward-mode.")))

  (testing "multiple input, vector output"
    (let [f (fn [a b c d e f]
              [(g/* (g/cos a) (g/cos b))
               (g/* (g/cos c) (g/cos d))
               (g/* (g/cos e) (g/cos f))])
          expected (g/simplify
                    ((D (D f)) 'a 'b 'c 'd 'e 'f))]
      (is (= expected
             (g/simplify
              ((t/gradient (t/gradient f)) 'a 'b 'c 'd 'e 'f)))
          "multivariable derivatives match (reverse-over-reverse)")

      (is (= expected
             (g/simplify
              ((D (t/gradient f)) 'a 'b 'c 'd 'e 'f)))
          "forward-over-reverse")

      ;; TODO enable this when we add support for tape and gradient comms in
      ;; lift-2.
      #_
      (is (= expected
             (g/simplify
              ((t/gradient (D f)) 'a 'b 'c 'd 'e 'f)))
          "reverse-over-forward"))))
