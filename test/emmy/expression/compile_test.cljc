#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.compile-test
  (:require [clojure.test :refer [is deftest testing]]
            [emmy.abstract.number]
            [emmy.expression.analyze :as a]
            [emmy.expression.compile :as c]
            [emmy.generic :as g]
            [emmy.structure :refer [up down]]
            [emmy.value :as v]
            [same :refer [ish?]])
  #?(:clj
     (:import (clojure.lang ExceptionInfo))))

(deftest mode-binding-test
  (testing "set-compiler-mode! works"
    (let [current-mode c/*mode*]
      (doseq [mode c/valid-modes]
        (c/set-compiler-mode! mode)
        (is (= mode (c/compiler-mode))
            "set-compiler-mode! works"))
      (c/set-compiler-mode! current-mode)))

  (doseq [mode c/valid-modes]
    (binding [c/*mode* mode]
      (is (= mode (c/compiler-mode))
          "valid modes are all returned by compiler-mode.")))

  (binding [c/*mode* :TOTALLY-INVALID]
    (is (thrown? ExceptionInfo
                 (c/compiler-mode))
        "invalid modes throw.")))

(deftest compile-fn-test
  (testing "argv generation"
    (let [f (fn [m]
              (fn [[t _ _]]
                (g/* m t)))
          compile (fn [opts]
                    (c/compile-state-fn f '[m] (up 't (up 'x0 'x1) (up 'v0 'v1))
                                        (merge {:gensym-fn (a/monotonic-symbol-generator 1)}
                                               opts)))]
      (is (= `(fn [[~'y1 [~'y2 ~'y3] [~'y4 ~'y5]] [~'p6]] (* ~'p6 ~'y1))
             (compile {:mode :clj :calling-convention :structure})))
      (is (= `(fn [~'a7 ~'a8 ~'a9]
                (let [~'y1 (aget ~'a7 0)
                      ~'y2 (aget ~'a7 1)
                      ~'y3 (aget ~'a7 2)
                      ~'y4 (aget ~'a7 3)
                      ~'y5 (aget ~'a7 4)
                      ~'p6 (aget ~'a9 0)]
                  (doto ~'a8 (aset 0 (* ~'p6 ~'y1)))))
             (compile {:mode :clj :calling-convention :primitive})))
      (is (= ["[y1, [y2, y3], [y4, y5]]" "[p6]" "  return p6 * y1;"]
             (compile {:mode :js :calling-convention :structure})))
      (is (= ["a7"
              "a8"
              "a9"
              (str
               "  const y1 = a7[0];\n"
               "  const y2 = a7[1];\n"
               "  const y3 = a7[2];\n"
               "  const y4 = a7[3];\n"
               "  const y5 = a7[4];\n"
               "  const p6 = a9[0];\n"
               "  a8[0] = p6 * y1;")]
             (compile {:mode :js :calling-convention :primitive})))
      (is (thrown? ExceptionInfo
                   (compile {:calling-convention :bogus}))
          "unknown calling convention will throw")))
  (testing "state-fn compilation"
    (let [f (fn [scale]
              (fn [[t]]
                (up (g/* scale (g/+ t (g// 1 2))))))
          params [10]
          initial-state [3]
          expected ((apply f params) initial-state)]
      (testing "compilation works in sci and native modes"

        (binding [c/*mode* :native]
          (is (= expected
                 ((c/compile-state-fn f params initial-state)
                  initial-state params))))

        (binding [c/*mode* :sci]
          (is (= expected
                 ((c/compile-state-fn f params initial-state)
                  initial-state params))))

        (testing "bind gensym to `identity` so we can check the result (multi language)"
          (doseq [[mode expected-source] {:clj `(fn [[~'y] [~'p]]
                                                  (vector (+ (* ~'p ~'y) (* 0.5 ~'p))))
                                          :js ["[y]" "[p]" "  return [p * y + 0.5 * p];"]}]
            (binding [c/*mode* mode]
              (let [compiler #(c/compile-state-fn
                               f params initial-state
                               {:gensym-fn identity})
                    f-source (compiler)]
                (is (= expected-source f-source)
                    "source code for your native language!")

                (binding [c/*mode* :native]
                  (is (= expected-source
                         (c/compile-state-fn
                          f params initial-state
                          {:gensym-fn identity
                           :mode mode}))
                      "explicit `:mode` overrides the dynamic binding."))

                (is (thrown? ExceptionInfo
                             (c/compile-state-fn
                              f params initial-state
                              {:gensym-fn identity
                               :mode :invalid}))
                    "explicit invalid modes will throw!")))

            (is (= expected ((c/sci-eval (c/compile-state-fn
                                          f params initial-state
                                          {:mode :sci}))
                             initial-state params))
                "source compiles to SCI and gives us the desired result.")))))

    (testing "compile-state-fn options"
      (binding [c/*mode* :source]
        (let [f (fn [scale]
                  (fn [[t]]
                    (up (g/* scale (g/+ t (g// 1 2))))))
              params [3.1]
              initial-state (up 1 (down 2 (down 4 (up 1))))]


          (doseq [[mode expected-source]
                  {:clj `(fn ~'[[y1 [y2 [y3 [y4]]]]]
                           (vector (+ (* 3.1 ~'y1) 1.55)))
                   :js ["[y1, [y2, [y3, [y4]]]]" "  return [3.1 * y1 + 1.55];"]}]
            (is (= expected-source
                   (c/compile-state-fn
                    f params initial-state
                    {:calling-convention :structure
                     :generic-params? false
                     :gensym-fn (a/monotonic-symbol-generator 1)
                     :mode mode}))
                "nested argument vector, no params."))

          (doseq [[mode expected-source]
                  {:clj `(fn ~'[[y1 [y2 [y3 [y4]]]] [p5]]
                           (vector (+ (* ~'p5 ~'y1) (* 0.5 ~'p5))))
                   :js ["[y1, [y2, [y3, [y4]]]]" "[p5]" "  return [p5 * y1 + 0.5 * p5];"]}]
            (is (= expected-source
                   (c/compile-state-fn
                    f params initial-state
                    {:calling-convention :structure
                     :generic-params? true
                     :gensym-fn (a/monotonic-symbol-generator 1)
                     :mode mode}))
                "nested argument vector, params."))))))

  (testing "non-state-fns"
    (let [f (fn [x] (up (g/+ (g/cube x) (g/sin x))))
          expected (up 999.4559788891106)]
      (testing "compilation works in sci and native modes"
        #?(:clj
           (binding [c/*mode* :native]
             (is (ish? expected
                       ((c/compile-fn f) 10)))))

        (binding [c/*mode* :sci]
          (is (ish? expected
                    ((c/compile-fn f) 10))))

        (testing "bind gensym to `identity` so we can check the result."
          (binding [c/*mode* :source]
            (let [[f-source clj-source] (with-redefs [gensym (fn
                                                               ([] (clojure.core/gensym))
                                                               ([x] x))]
                                          [(c/compile-fn f)
                                           (binding [c/*mode* :clj] (c/compile-fn f))])]
              (is (= #?(:clj `(fn [~'y0001]
                                (vector
                                 (+ (~'Math/pow ~'y0001 3.0)
                                    (~'Math/sin ~'y0001))))
                        :cljs ["y0001" "  return [Math.pow(y0001, 3) + Math.sin(y0001)];"])
                     f-source)
                  "source code in your native language!")

              (is (= expected ((c/sci-eval clj-source) 10))
                  "source compiles to SCI and gives us the desired result."))

            (with-redefs [gensym (fn
                                   ([] (clojure.core/gensym))
                                   ([x] x))]
              (is (= #?(:clj `(fn [~'y0001] (+ (* -1.0 ~'y0001) 28.0))
                        :cljs ["y0001" "  return - y0001 + 28;"])
                     (c/compile-fn
                      (fn [x]
                        (g/- (g/* 8 (g/+ (g// 1 2) 3))
                             x))))
                  "fractional arithmetic results in double literals.")

              (is (= #?(:clj `(fn [~'y0001] (vector 2.0 (+ ~'y0001 0.5)))
                        :cljs ["y0001" "  return [2, y0001 + 0.5];"])
                     (c/compile-fn
                      (fn [x]
                        (up 2 (g/+ (g// 1 2) x)))))
                  "`(/ 1 2)` is resolved into 0.5 at compile time.")))))))

  (let [f          (fn [x] (g/+ 1 (g/square (g/sin x))))
        cf         (c/compile-fn f)
        cf2        (c/compile-fn f)
        cf-nocache (c/compile-fn f 1 {:cache false})]
    (is (= (f 0.5)
           (cf 0.5)
           (cf2 0.5)
           (cf-nocache 0.5))
        "the fn has no simplifications available so the results are identical;
        the compiled fn is faster."))

  (testing "multivariate function, arity detection"
    (let [f3 (fn [x y z]
               (g/sqrt
                (g/+ (g/square x)
                     (g/square y)
                     (g/square z))))]
      (is (= (f3 1 2 3)
             ((c/compile-fn f3) 1 2 3)
             ((c/compile-fn f3) 1 2 3)
             ((c/compile-fn f3 3 {:cache false}) 1 2 3))
          "multi-arity functions work.")))

  (testing "compile-fn can only detect single-arity fns"
    (let [f (fn
              ([x] x)
              ([x y z]
               (g/sqrt
                (g/+ (g/square x)
                     (g/square y)
                     (g/square z)))))]
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (c/compile-fn f))
          "you have to specify an arity for compile-fn to work on a multi-arity
          fn.")
      (is (= 1
             (f 1)
             ((c/compile-fn f 1) 1))
          "If you specify an arity, you avoid the error.")

      (is (== 3 (f 1 2 2)))
      (is (== (f 1 2 2) ((c/compile-fn f 3) 1 2 2))
          "If you specify an arity, you avoid the error.")))

  (testing "simplify?"
    (let [f (fn [x]
              (g/+ (g/square (g/sin x))
                   (g/square (g/cos x))))
          one #?(:clj 1.0 :cljs 1)
          two #?(:clj 2.0 :cljs 2)]
      (is (= `(fn [~'y0001] ~one)
             (c/compile-fn f 1 {:mode :clj :simplify? true}))
          "simplify? true triggers body simplification.")

      (is (= `(fn [~'y0001]
                (+ (~'Math/pow (~'Math/sin ~'y0001) ~two)
                   (~'Math/pow (~'Math/cos ~'y0001) ~two)))
             (c/compile-fn f 1 {:mode :clj :simplify? false}))
          "simplify? false leaves body untouched."))))

(deftest compile-state-tests
  (let [f  (fn [[[a b] [c d]]]
             (g/- (g/* a d) (g/* b c)))
        sf (fn [k] (fn [s] (g/* k (f s))))
        s (up (down 2 3) (down 4 5))
        t (up (down 3 4) (down -1 2))]
    (testing "non-compiled, generic state function results"
      (is (= -2 (f s)))
      (is (= 10 (f t)))
      (is (= -4 ((sf 2) s)))
      (is (= 20 ((sf 2) t))))

    (testing "compiled state function matches the original (structure)."
      (let [cf (c/compile-state-fn sf [1] s {:calling-convention :structure})]
        #?(:clj  (do (is (v/= ((sf 1) s) (cf s [1])))
                     (is (v/= ((sf 1) t) (cf t [1])))
                     (is (v/= ((sf 2) s) (cf s [2])))
                     (is (v/= ((sf 2) t) (cf t [2]))))
           :cljs (let [sj (clj->js s)
                       tj (clj->js t)]
                   ;; in Clojurescript, we need something "iterable in the JavaScript sense"
                   ;; if we're going to use argument destructuring in a compiled function:
                   ;;   (def z (js/Function "[a, b]" "return {a:a, b:b};"))
                   ;;     #'cljs.user/z
                   ;;   (z [1 2])
                   ;;     #js {:a 1, :b 2}             ; OK
                   ;;   (z (up 1 2))
                   ;;     object is not iterable...    ; NG
                   ;;   (z (clj->js (up 1 2)))
                   ;;     #js {:a 1, :b 2}             ; OK
                   (is (v/= ((sf 1) s) (cf sj [1])))
                   (is (v/= ((sf 1) t) (cf tj [1])))
                   (is (v/= ((sf 2) s) (cf sj [2])))
                   (is (v/= ((sf 2) t) (cf tj [2])))))))))
