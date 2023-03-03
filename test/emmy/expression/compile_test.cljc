#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.compile-test
  (:require [clojure.test :refer [is deftest testing]]
            [emmy.abstract.number]
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
  (testing "state-fn compilation"
    (let [f (fn [scale]
              (fn [[t]]
                (up (g/* scale (g/+ t (g// 1 2))))))
          params [10]
          initial-state [3]
          expected ((apply f params) initial-state)]
      (testing "compilation works in sci and native modes"
        #?(:clj
           (binding [c/*mode* :native]
             (is (= expected
                    ((c/compile-state-fn* f params initial-state)
                     initial-state params)))))

        (binding [c/*mode* :sci]
          (is (= expected
                 ((c/compile-state-fn* f params initial-state)
                  initial-state params))))

        (testing "bind gensym to `identity` so we can check the result."
          (binding [c/*mode* :source]
            (let [compiler #(c/compile-state-fn*
                             f params initial-state
                             {:gensym-fn identity})
                  f-source (compiler)
                  clj-source (binding [c/*mode* :clj]
                               (compiler))]
              (println "clj-source " clj-source)
              (is (= #?(:clj `(fn [[~'y] [~'p]]
                                (vector (+ (* ~'p ~'y) (* 0.5 ~'p))))
                        :cljs ["[y]" "[p]" "  return [p * y + 0.5 * p];"])
                     f-source)
                  "source code for your native language!")

              (binding [c/*mode* :native]
                (is (= #?(:clj `(fn [[~'y] [~'p]]
                                  (vector (+ (* ~'p ~'y) (* 0.5 ~'p))))
                          :cljs ["[y]" "[p]" "  return [p * y + 0.5 * p];"])
                       (c/compile-state-fn*
                        f params initial-state
                        {:gensym-fn identity
                         :mode :source}))
                    "explicit `:mode` overrides the dynamic binding."))

              (is (thrown? ExceptionInfo
                           (c/compile-state-fn*
                            f params initial-state
                            {:gensym-fn identity
                             :mode :invalid}))
                  "explicit invalid modes will throw!")

              (is (= expected ((c/sci-eval clj-source)
                               initial-state params))
                  "source compiles to SCI and gives us the desired result."))))))

    (testing "compile-state-fn options"
      (binding [c/*mode* :source]
        (let [gensym-fn (fn []
                          (let [i (atom 0)]
                            (fn [x]
                              (symbol
                               (str x (swap! i inc))))))
              f (fn [scale]
                  (fn [[t]]
                    (up (g/* scale (g/+ t (g// 1 2))))))
              params [3]
              initial-state (up 1 (down 2 (down 4 (up 1))))]

          (is (= #?(:clj `(fn ~'[[y1 [y2 [y3 [y4]]]]]
                            (vector (+ (* 3.0 ~'y1) 1.5)))
                    :cljs ["[y1, [y2, [y3, [y4]]]]" "  return [3 * y1 + 1.5];"])
                 (c/compile-state-fn*
                  f params initial-state
                  {:flatten? false
                   :generic-params? false
                   :gensym-fn (gensym-fn)}))
              "nested argument vector, no params.")

          (is (= #?(:clj `(fn ~'[[y1 [y2 [y3 [y4]]]] [p5]]
                            (vector (+ (* ~'p5 ~'y1) (* 0.5 ~'p5))))
                    :cljs  ["[y1, [y2, [y3, [y4]]]]" "[p5]" "  return [p5 * y1 + 0.5 * p5];"])
                 (c/compile-state-fn*
                  f params initial-state
                  {:flatten? false
                   :generic-params? true
                   :gensym-fn (gensym-fn)}))
              "nested argument vector, params.")))))

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
            ;; we compile twice because in mode :native we'd get JS source
            ;; in a JS environment, and that can't be evaluated by sci.
            ;; It may be that the sci environment has been made obsolete.
            (let [[f-source clj-source] (with-redefs [gensym (fn
                                                               ([] (clojure.core/gensym))
                                                               ([x] x))]
                             [(c/compile-fn f)
                              (binding [c/*mode* :clj] (c/compile-fn f))])]
              (is (= #?(:clj `(fn [~'x]
                                (vector
                                 (+ (~'Math/pow ~'x 3.0)
                                    (~'Math/sin ~'x))))
                        :cljs ["x" "  return [Math.pow(x, 3) + Math.sin(x)];"])
                     f-source)
                  "source code in your native language!")

              (is (= expected ((c/sci-eval clj-source) 10))
                  "source compiles to SCI and gives us the desired result."))

            (with-redefs [gensym (fn
                                   ([] (clojure.core/gensym))
                                   ([x] x))]
              (is (= #?(:clj `(fn [~'x] (+ (* -1.0 ~'x) 28.0))
                        :cljs ["x" "  return - x + 28;"])
                     (c/compile-fn
                      (fn [x]
                        (g/- (g/* 8 (g/+ (g// 1 2) 3))
                             x))))
                  "all remaining numerical literals are doubles.")

              (is (= #?(:clj `(fn [~'x] (vector 2.0 (+ ~'x 0.5)))
                        :cljs ["x" "  return [2, x + 0.5];"])
                     (c/compile-fn
                      (fn [x]
                        (up 2 (g/+ (g// 1 2) x)))))
                  "`(/ 1 2)` is resolved into 0.5 at compile time.")))))))

  (let [f          (fn [x] (g/+ 1 (g/square (g/sin x))))
        cf         (c/compile-fn f)
        cf2        (c/compile-fn f)
        cf-nocache (c/compile-fn* f)]
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
             ((c/compile-fn* f3) 1 2 3))
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

      (is (== 3.0 (f 1 2 2)))
      (is (== (f 1 2 2) ((c/compile-fn f 3) 1 2 2))
          "If you specify an arity, you avoid the error."))))

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

    (testing "compiled state function matches the original."
      (let [cf (c/compile-state-fn sf [1] s)]
        (is (v/= ((sf 1) s) (cf (flatten s) [1])))
        (is (v/= ((sf 1) t) (cf (flatten t) [1])))
        (is (v/= ((sf 2) s) (cf (flatten s) [2])))
        (is (v/= ((sf 2) t) (cf (flatten t) [2])))))))
