#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.dual-test
  (:require #?(:cljs [emmy.util :as u])
            [clojure.test :refer [is deftest testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.autodiff :as ad]
            [emmy.dual :as d]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.numerical.derivative :refer [D-numeric]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]
            [same.core :refer [ish? with-comparator]]))

(use-fixtures :each hermetic-simplify-fixture)

(defn nonzero [gen]
  (gen/fmap (fn [x]
              (if (= x 0)
                (g/one-like x)
                x))
            gen))

(def real-dual-gen
  (sg/dual))

(def integral-dual-gen
  (sg/dual gen/nat))

(deftest dual-type-tests
  (defmethod g/zero? [#?(:clj String :cljs js/String)] [_] false)
  (checking "native comparison operators work with dual" 100
            [l sg/real, r sg/real]
            (is (= (v/compare l r)
                   (v/compare (d/bundle-element l 1 0) r)
                   (v/compare l (d/bundle-element r 1 0)))))

  #?(:cljs
     (testing "comparison unit tests"
       (is (= 0 (d/bundle-element 0 1 0)))
       (is (= (u/bigint 0) (d/bundle-element 0 1 0)))
       (is (= (u/long 0) (d/bundle-element 0 1 0)))
       (is (= (u/int 0) (d/bundle-element 0 1 0)))))

  #?(:cljs
     ;; NOTE: JVM Clojure won't allow non-numbers on either side of < and
     ;; friends. Once we implement `v/<` we can duplicate this test for those
     ;; overridden versions, which should piggyback on compare.
     (let [real-minus-rationals (gen/one-of [sg/any-integral (sg/reasonable-double)])]
       (checking "[[emmy.dual/Dual]] is transparent to native comparison operators" 100
                 [[l-num r-num] (gen/vector real-minus-rationals 2)]
                 (let [compare-bit (v/compare l-num r-num)]
                   (doseq [l [l-num (d/bundle-element l-num 1 0)]
                           r [r-num (d/bundle-element r-num 1 0)]]
                     (cond (neg? compare-bit)  (is (< l r))
                           (zero? compare-bit) (is (and (<= l r) (= l r) (>= l r)))
                           :else (is (> l r))))))))

  (checking "v/numerical?, v/scalar?" 100 [diff (sg/dual sg/real)]
            (is (v/scalar? diff)
                "True for all duals"))

  (testing "value protocol implementation"
    (let [zero (d/bundle-element 0 0 0)
          dy   (d/bundle-element 0 1 0)]
      (is (g/zero? zero)
          "zero? returns true for an empty term list")

      (is (not (g/zero? dy))
          "the finite term is 0, but `g/zero?` fails if any perturbation is
          non-zero.")

      (is (= dy 0)
          "subtly, `dy` IS in fact equal to zero; this can be used for control
      flow.")

      (testing "g/one? only responds true to a one primal if tangent is zero."
        (is (g/one? (d/bundle-element 1 0 0)))
        (is (not (g/one?  (d/bundle-element 1 1 0)))))

      (testing
          "g/identity? only responds true to an `identity` primal if the tangent
      is zero."
        (is (g/identity? (d/bundle-element 1 0 0)))
        (is (not (g/identity? (d/bundle-element 1 1 0)))))

      (checking "*-like works" 100 [diff real-dual-gen]
                (is (g/zero? (g/zero-like diff)))
                (is (g/one? (g/one-like diff)))
                (is (g/identity? (g/identity-like diff))))

      (testing "equality, comparison"
        (checking "g/negative?, g/infinite?" 100 [x sg/real]
                  (let [elem (d/bundle-element x 1 0)]
                    (is (= (g/negative? x)
                           (g/negative? elem))
                        "negative? operates on primal only.")

                    (is (not (g/infinite? elem))
                        "infinite? is always false for real finite parts.")))

        (testing "g/infinite?"
          (is (not (g/infinite? (d/bundle-element 10 ##Inf 0)))
              "g/infinite? only looks at the finite part right now. Not sure how
              we would get into an infinite derivative with non-infinite finite
              part, but marking this test here as documentation.")

          (is (every?
               g/infinite?
               [(d/bundle-element ##-Inf 1 0)
                (d/bundle-element ##Inf 1 0)])
              "an infinite or negative infinite value in the finite part slot
               makes the dual `g/infinite?`"))

        (checking "=, equiv ignore tangent parts" 100
                  [n sg/real-without-ratio]
                  (is (= (d/bundle-element n 1 0) n)
                      "dual on the left works.")

                  #?(:cljs
                     (is (= n (d/bundle-element n 1 0))
                         "dual on the right works in CLJS."))

                  (is (d/equiv (d/bundle-element n 1 0) n n (d/bundle-element n 1 0) n)
                      "d/equiv matches = behavior, varargs"))

        (checking "eq uses tangent" 100 [n sg/real]
                  (is (d/eq (d/bundle-element n 0 0) n n (d/bundle-element n 0 0) n)
                      "eq is true with no tangent, varargs")

                  (is (not (d/eq (d/bundle-element n 1 0) n))
                      "eq is FALSE with a tangent, bundle on left")

                  (is (not (d/eq n (d/bundle-element n 1 0)))
                      "eq is FALSE with a tangent, bundle on right")

                  (is (d/eq (d/bundle-element n 1 0) (d/bundle-element n 1 0))
                      "d/eq handles equality")

                  (is (not (d/eq (d/bundle-element n 1 0) (d/bundle-element n 2 0)))
                      "d/eq is false for [[emmy.dual/Dual]]s with diff tags"))

        (checking "compare ignores tangent" 100
                  [l sg/real, r sg/real]
                  (is (= (v/compare l r)
                         (v/compare (d/bundle-element l 1 0) r)
                         (v/compare l (d/bundle-element r 1 0)))
                      "dual works on either side.")

                  (is (= (d/compare l r)
                         (d/compare (d/bundle-element l r 0) r)
                         (d/compare l (d/bundle-element r l 0)))
                      "d/compare can handle non-dual on either side, also
                    ignores tangents.")

                  (testing "{d,v}/compare l, l matches equals behavior, ignores tangents"
                    (is (zero? (d/compare (d/bundle-element l r 0) l)))
                    (is (zero? (d/compare l (d/bundle-element l r 0))))
                    (is (zero? (v/compare (d/bundle-element l r 0) l)))
                    (is (zero? (v/compare l (d/bundle-element l r 0))))))

        (checking "compare-full takes tags into account" 100
                  [l sg/real]
                  (is (pos? (d/compare-full (d/bundle-element l 1 0) l))
                      "a [[emmy.dual/Dual]] with a positive tangent is ALWAYS
                    greater than a non-[[emmy.dual/Dual]] whatever the tangent.")

                  (is (neg? (d/compare-full l (d/bundle-element l 1 0)))
                      "a [[emmy.dual/Dual]] with a positive tangent is ALWAYS
                    greater than a non-[[emmy.dual/Dual]] whatever the tangent."))

        (testing "freeze, simplify, str"
          (let [not-simple (g/square
                            (g/square (d/bundle-element 'x 1 0)))]
            (is (= '[Dual 0
                     (expt x 4)
                     (* 2 (expt x 2) 2 x)]
                   (g/freeze not-simple))
                "A frozen dual freezes primal and tangent")

            (is (= '[Dual 0
                     (expt x 4)
                     (* 4 (expt x 3))]
                   (g/freeze
                    (g/simplify not-simple)))
                "simplify simplifies primal and tangent")

            (is (= "#emmy.dual.Dual{:tag 0, :primal (expt x 4), :tangent (* 4 (expt x 3))}"
                   (str (g/simplify not-simple)))
                "str representation properly simplifies.")))))))

(deftest dual-fn-tests
  (testing "duals can take branches inside functions, PROVIDED (with
            clojure.core/=) the perturbed variable is on the
            left! (ClojureScript can handle equals on either side.)"
    (let [f (fn [x]
              (let [g (if #?(:clj (= x 10) :cljs (= 10 x))
                        (g/* x g/square)
                        (g/* x g/cube))]
                (g x)))
          Df (d/derivative f)]
      (is (= ((d/derivative (g/* identity g/square)) 10)
             (Df 10))
          "providing 10 takes the x*g/square branch")
      (is (= ((d/derivative (g/* identity g/cube)) 9)
             (Df 9))
          "providing 9 takes the x*g/cube branch"))))

(deftest active-tag-tests
  (testing "with-active-tag works"
    (is (not (d/tag-active? 'tag))
        "this tag is not active if it's not bound.")

    (is (= 6 (d/with-active-tag 'tag
               (fn [& xs]
                 (is (d/tag-active? 'tag)
                     "the supplied tag is active inside the scope of `f`.")
                 (reduce + xs))
               [1 2 3]))
        "d/with-active-tag calls its fn with the supplied args.")))

(deftest dual-arithmetic-tests
  (let [Df (d/derivative (fn [_x]))]
    (checking "derivative of nil-valued function is always zero" 100
              [x sg/real]
              (is (zero? (Df x)))))

  (checking "(a/b)*b == a, (a*b)/b == a" 100
            [x integral-dual-gen
             y (nonzero integral-dual-gen)]
            (is (d/eq x (g/* (g// x y) y)))
            (is (d/eq x (g// (g/* x y) y))))

  (checking "solve-linear, div relationships" 100
            [x  real-dual-gen
             y (nonzero sg/real)]
            (let [y (d/bundle-element y 1 0)]
              (is (d/eq (g/solve-linear-left y x)
                        (g// x y)))

              (is (d/eq (g/solve-linear-left y x)
                        (g/solve-linear-right x y)))

              (is (d/eq (g/solve-linear-left y x)
                        (g/solve-linear y x)))))

  (testing "various unit tests with more terms"
    (let [tangent  (fn [dx] (d/tangent dx 0))
          simplify (comp g/simplify tangent)]
      (is (v/= '(* 3 (expt x 2))
               (simplify
                (g/expt (g/+ 'x (d/bundle-element 0 1 0)) 3))))

      (is (v/= '(* 4 (expt x 3))
               (simplify
                (g/expt (g/+ 'x (d/bundle-element 0 1 0)) 4))))

      (let [dx   (d/bundle-element 0 1 0)
            x+dx (g/+ 'x dx)
            f    (fn [x] (g/* x x x x))]
        (is (v/= '(* 4 (expt x 3))
                 (simplify (g/* x+dx x+dx x+dx x+dx))))
        (is (v/= '(* 12 (expt x 2))
                 (simplify
                  (g/+ (g/* (g/+ (g/* (g/+ x+dx x+dx) x+dx)
                                 (g/* x+dx x+dx))
                            x+dx)
                       (g/* x+dx x+dx x+dx)))))

        (is (v/= '(* 24 x)
                 (simplify
                  (g/+
                   (g/* (g/+ (g/* 2 x+dx)
                             x+dx x+dx x+dx x+dx) x+dx)
                   (g/* (g/+ x+dx x+dx) x+dx)
                   (g/* x+dx x+dx)
                   (g/* (g/+ x+dx x+dx) x+dx)
                   (g/* x+dx x+dx)))))

        (is (= 24 (tangent
                   (g/+ (g/* 6 x+dx)
                        (g/* 2 x+dx)
                        x+dx x+dx x+dx x+dx
                        (g/* 2 x+dx)
                        x+dx x+dx x+dx x+dx
                        (g/* 2 x+dx)
                        x+dx x+dx x+dx x+dx))))

        (is (v/= '(* 4 (expt x 3))
                 (simplify (f x+dx))))))))

(deftest dual-api-tests
  (checking "check accessors for bundle-element" 100
            [primal  (sg/reasonable-double)
             tangent (sg/reasonable-double)
             tag     gen/nat]
            (let [diff (d/bundle-element primal tangent tag)]
              (is (= primal (d/primal diff)))
              (is (= tangent (d/tangent diff)))
              (is (= tag (d/tag diff)))))

  (testing "bundle-element can flatten bundle-element primal, tangent"
    (is (d/eq (g/+ (g/sin (d/bundle-element 1 1 0))
                   (g/* (g/cos (d/bundle-element 1 1 0))
                        (d/bundle-element 0 1 1)))
              (d/bundle-element (g/sin (d/bundle-element 1 1 0))
                                (g/cos (d/bundle-element 1 1 0))
                                1))))

  (checking "(primal-tangent-pair diff) == [(primal diff) (tangent diff)]" 100
            [diff real-dual-gen]
            (let [[primal tangent] (d/primal-tangent-pair diff)]
              (is (d/eq primal (d/primal diff)))
              (is (d/eq tangent (d/tangent diff))))

            ;; with specified tag
            (let [tag (d/tag diff)
                  [primal tangent] (d/primal-tangent-pair diff tag)]
              (is (d/eq primal (d/primal diff tag)))
              (is (d/eq tangent (d/tangent diff tag))))))

(deftest lifted-fn-tests
  (letfn [(breaks? [f x]
            (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                         ((d/derivative f) x))))]
    (checking "integer-discontinuous derivatives work" 100 [x sg/real]
              (if (v/integral? x)
                (do (breaks? g/floor x)
                    (breaks? g/ceiling x)
                    (breaks? g/integer-part x)
                    (breaks? g/fractional-part x))

                (do (is (zero? ((d/derivative g/floor) x)))
                    (is (zero? ((d/derivative g/ceiling) x)))
                    (is (zero? ((d/derivative g/integer-part) x)))
                    (is (g/one? ((d/derivative g/fractional-part) x)))))))

  (testing "lift-n"
    (let [*   (ad/lift-n g/* (fn [_] 1) (fn [_ y] y) (fn [x _] x))
          Df7 (d/derivative
               (fn x**7 [x] (* x x x x x x x)))
          Df1 (d/derivative *)
          Df0 (d/derivative (fn [_] (*)))]
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
          Df         (d/derivative f)
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
  (is (zero? ((d/derivative g/sinc) 0)))
  (is (zero? ((d/derivative g/tanc) 0)))
  (is (zero? ((d/derivative g/sinhc) 0)))
  (is (zero? ((d/derivative g/tanhc) 0)))

  (letfn [(gen-double [min max]
            (gen/double*
             {:infinite? false
              :NaN? false
              :min min
              :max max}))]
    (with-comparator (v/within 1e-4)
      (checking "sinc" 100 [n (gen-double 1 50)]
                (is (ish? ((D-numeric g/sinc) n)
                          ((d/derivative g/sinc) n))))

      ;; attempting to limit to a region where we avoid the infinities at
      ;; multiples of pi/2 (other than 0).
      (checking "tanc" 100 [n (gen-double 0.01 (- (/ Math/PI 2) 0.01))]
                (is (ish? ((D-numeric g/tanc) n)
                          ((d/derivative g/tanc) n))))

      (checking "tanhc" 100 [n (gen-double 1 50)]
                (is (ish? ((D-numeric g/tanhc) n)
                          ((d/derivative g/tanhc) n)))))

    (with-comparator (v/within 1e-4)
      (checking "sinhc" 100 [n (gen-double 1 10)]
                (is (ish? ((D-numeric g/sinhc) n)
                          ((d/derivative g/sinhc) n)))))

    (with-comparator (v/within 1e-8)
      (checking "acot" 100 [n (gen-double 0.01 (- (/ Math/PI 2) 0.01))]
                (is (ish? ((D-numeric g/acot) n)
                          ((d/derivative g/acot) n))))

      (checking "asec" 100 [n (gen-double 3 100)]
                (is (ish? ((D-numeric g/asec) n)
                          ((d/derivative g/asec) n))))

      (checking "acsc" 100 [n (gen-double 3 100)]
                (is (ish? ((D-numeric g/acsc) n)
                          ((d/derivative g/acsc) n))))

      (checking "sech" 100 [n (gen-double 3 100)]
                (is (ish? ((D-numeric g/sech) n)
                          ((d/derivative g/sech) n))))

      (checking "coth" 100 [n (gen-double 1 3)]
                (is (ish? ((D-numeric g/coth) n)
                          ((d/derivative g/coth) n))))

      (checking "csch" 100 [n (gen-double 0.5 10)]
                (is (ish? ((D-numeric g/csch) n)
                          ((d/derivative g/csch) n))))

      (checking "acosh" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/acosh) n)
                          ((d/derivative g/acosh) n))))

      (checking "asinh" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/asinh) n)
                          ((d/derivative g/asinh) n))))

      (checking "atanh" 100 [n (gen-double 0.1 0.9)]
                (is (ish? ((D-numeric g/atanh) n)
                          ((d/derivative g/atanh) n))))

      (checking "acoth" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/acoth) n)
                          ((d/derivative g/acoth) n))))

      (checking "asech" 100 [n (gen-double 0.1 0.9)]
                (is (ish? ((D-numeric g/asech) n)
                          ((d/derivative g/asech) n))))

      (checking "acsch" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/acsch) n)
                          ((d/derivative g/acsch) n)))))))
