#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.compile-test
  (:require [clojure.string :as s]
            [clojure.test :refer [is deftest testing]]
            [emmy.abstract.number]
            [emmy.expression.analyze :as a]
            [emmy.expression.compile :as c]
            [emmy.generic :as g]
            [emmy.structure :refer [up down]]
            [emmy.util :as u]
            [emmy.value :as v]
            [same.core :refer [ish?]])
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
      (is (= `(fn [[~'y1 [~'y2 ~'y3] [~'y4 ~'y5]] [~'p6]] (let [~'_7 (* ~'p6 ~'y1)] ~'_7))
             (compile {:mode :clj :calling-convention :structure})))
      (is (= '(fn [a07 a08 a09]
                (let [y01 (aget a07 0)
                      y02 (aget a07 1)
                      y03 (aget a07 2)
                      y04 (aget a07 3)
                      y05 (aget a07 4)
                      p06 (aget a09 0)
                      _10 (* p06 y01)]
                  _10))
              (u/without-symbol-namespaces
               (compile {:mode :clj
                         :calling-convention :primitive
                         :gensym-fn (a/monotonic-symbol-generator 2)}))))
      (is (= ["[y1, [y2, y3], [y4, y5]]" "[p6]" (s/join "\n" ["  const _7 = p6 * y1;"
                                                              "  return _7;"])]
             (compile {:mode :js :calling-convention :structure})))
      (is (= ["a07"
              "a08"
              "a09"
              (s/join "\n" ["  const y01 = a07[0];"
                            "  const y02 = a07[1];"
                            "  const y03 = a07[2];"
                            "  const y04 = a07[3];"
                            "  const y05 = a07[4];"
                            "  const p06 = a09[0];"
                            "  const _10 = p06 * y01;"
                            "  return _10;" ])]
             (compile {:mode :js
                       :calling-convention :primitive
                       :gensym-fn (a/monotonic-symbol-generator 2)})))
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

        (testing "bind gensym so we can check the result (multi language)"
          (doseq [[mode expected-source] {:clj '(fn [[y1] [p2]]
                                                  (let [_3 (* p2 y1)
                                                        _4 0.5
                                                        _5 (* _4 p2)
                                                        _6 (+ _3 _5)
                                                        _7 (vector _6)]
                                                    _7))
                                          :js ["[y1]" "[p2]"
                                               (s/join "\n" ["  const _3 = p2 * y1;"
                                                             "  const _4 = 0.5;"
                                                             "  const _5 = _4 * p2;"
                                                             "  const _6 = _3 + _5;"
                                                             "  const _7 = [_6];"
                                                             "  return _7;"])]}]
            (binding [c/*mode* mode]

              (let [gensym-fn (a/monotonic-symbol-generator 1)
                    compiler #(u/without-symbol-namespaces
                               (c/compile-state-fn
                                f params initial-state
                                {:gensym-fn gensym-fn}))
                    f-source (compiler)]
                (is (= expected-source f-source)
                    "source code for your native language!")

                (binding [c/*mode* :native]
                  (is (= expected-source
                         (u/without-symbol-namespaces
                          (c/compile-state-fn
                           f params initial-state
                           {:gensym-fn gensym-fn
                            :mode mode})))
                      "explicit `:mode` overrides the dynamic binding."))

                (is (thrown? ExceptionInfo
                             (c/compile-state-fn
                              f params initial-state
                              {:gensym-fn gensym-fn
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
                  {:clj '(fn [[y1 [y2 [y3 [y4]]]]]
                           (let [_5 3.1
                                 _6 (* _5 y1)
                                 _7 1.55
                                 _8 (+ _6 _7)
                                 _9 (vector _8)]
                             _9))
                   :js ["[y1, [y2, [y3, [y4]]]]"
                        (s/join "\n" ["  const _5 = 3.1;"
                                      "  const _6 = _5 * y1;"
                                      "  const _7 = 1.55;"
                                      "  const _8 = _6 + _7;"
                                      "  const _9 = [_8];"
                                      "  return _9;"])]}]
            (is (= expected-source
                   (u/without-symbol-namespaces
                    (c/compile-state-fn
                     f params initial-state
                     {:calling-convention :structure
                      :generic-params? false
                      :gensym-fn (a/monotonic-symbol-generator 1)
                      :mode mode})))
                "nested argument vector, no params."))

          (doseq [[mode expected-source]
                  {:clj '(fn [[y01 [y02 [y03 [y04]]]] [p05]]
                         (let [_06 (* p05 y01)
                               _07 0.5
                               _08 (* _07 p05)
                               _09 (+ _06 _08)
                               _10 (vector _09)]
                           _10))
                   :js ["[y01, [y02, [y03, [y04]]]]" "[p05]"
                        (s/join "\n" ["  const _06 = p05 * y01;"
                                      "  const _07 = 0.5;"
                                      "  const _08 = _07 * p05;"
                                      "  const _09 = _06 + _08;"
                                      "  const _10 = [_09];"
                                      "  return _10;"])]}]
            (is (= expected-source
                   (u/without-symbol-namespaces
                    (c/compile-state-fn
                     f params initial-state
                     {:calling-convention :structure
                      :generic-params? true
                      :gensym-fn (a/monotonic-symbol-generator 2)
                      :mode mode})))
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
                                          [(u/without-symbol-namespaces (c/compile-fn f))
                                           (binding [c/*mode* :clj] (c/compile-fn f))])]
              (is (= #?(:clj '(fn [y0001]
                                (let [_0002 3.0
                                      _0003 (pow y0001 _0002)
                                      _0004 (sin y0001)
                                      _0005 (+ _0003 _0004)
                                      _0006 (vector _0005)]
                                  _0006))
                        :cljs ["y0001"
                               (s/join "\n" ["  const _0002 = 3;"
                                             "  const _0003 = Math.pow(y0001, _0002);"
                                             "  const _0004 = Math.sin(y0001);"
                                             "  const _0005 = _0003 + _0004;"
                                             "  const _0006 = [_0005];"
                                             "  return _0006;"])])
                     f-source)
                  "source code in your native language!")

              (is (= expected ((c/sci-eval clj-source) 10))
                  "source compiles to SCI and gives us the desired result."))

            (with-redefs [gensym (fn
                                   ([] (clojure.core/gensym))
                                   ([x] x))]
              (is (= #?(:clj '(fn [y0001]
                                (let [_0002 -1.0
                                      _0003 (* _0002 y0001)
                                      _0004 28.0
                                      _0005 (+ _0003 _0004)]
                                  _0005))
                        :cljs ["y0001"
                               (s/join "\n" ["  const _0002 = -1;"
                                             "  const _0003 = _0002 * y0001;"
                                             "  const _0004 = 28;"
                                             "  const _0005 = _0003 + _0004;"
                                             "  return _0005;"])])
                     (u/without-symbol-namespaces
                      (c/compile-fn
                       (fn [x]
                         (g/- (g/* 8 (g/+ (g// 1 2) 3))
                              x)))))
                  "fractional arithmetic results in double literals.")

              (is (= #?(:clj '(fn [y0001]
                                (let [_0002 2.0
                                      _0003 0.5
                                      _0004 (+ y0001 _0003)
                                      _0005 (vector _0002 _0004)]
                                  _0005))
                        :cljs ["y0001"
                               (s/join "\n" ["  const _0002 = 2;"
                                             "  const _0003 = 0.5;"
                                             "  const _0004 = y0001 + _0003;"
                                             "  const _0005 = [_0002, _0004];"
                                             "  return _0005;"])])
                     (u/without-symbol-namespaces
                      (c/compile-fn
                       (fn [x]
                         (up 2 (g/+ (g// 1 2) x))))))
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
          one #?(:clj 1.0 :cljs 1)]
      (is (= `(fn [~'y0001] ~one)
             (c/compile-fn f 1 {:mode :clj :simplify? true}))
          "simplify? true triggers body simplification.")

      (is (= '(fn [y0001]
                (let [_0002 (sin y0001)
                      _0003 2.0
                      _0004 (pow _0002 _0003)
                      _0005 (cos y0001)
                      _0006 (pow _0005 _0003)
                      _0007 (+ _0004 _0006)]
                  _0007))
             (u/without-symbol-namespaces
              (c/compile-fn f 1 {:mode :clj :simplify? false})))
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
