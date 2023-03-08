#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.analyze-test
  (:require [clojure.string :as cs]
            [clojure.test :refer [is deftest testing]]
            [emmy.expression.analyze :as a]
            [emmy.polynomial :as poly]))

(deftest symbol-generator-test
  (let [gen (a/monotonic-symbol-generator 4 "cake")
        symbols (repeatedly 1000 gen)]
    (is (= symbols (sort symbols))
        "Generated symbols sort into the same order in which they were
        generated.")

    (is (every? #(cs/starts-with? (str %) "cake")
                symbols)
        "The prefix gets prepended to every generated symbol.")))

(deftest analyzer-test
  (testing "interactive expression analysis and simplification"
    (let [backend poly/analyzer
          gensym (a/monotonic-symbol-generator 16 "-s-")
          pa (a/make-analyzer backend gensym)
          simplify (a/expression-simplifier pa)
          simplify-and-reset (a/default-simplifier pa)
          analyze (a/expression-analyzer pa)
          get-tables (a/auxiliary-variable-fetcher pa)
          reset-tables! (a/initializer pa)]
      (is (= '(+ (* 3 x) (* 2 -s-0000000000000001))
             (analyze '(+ x x x (sin x) (sin x)))))

      (is (= '(+ (* 2 -s-0000000000000001)
                 -s-0000000000000002)
             (analyze '(+ (sin x) (sin x) (cos y))))
          "A second call uses the same variables for any subexpressions it's
    seen.")

      (is (= '(+ (* 3 x) (* 2 (sin x)))
             (simplify '(+ x x x (sin x) (sin x)))
             (simplify '(+ x x x (sin x) (sin x))))
          "two simplify calls return the same thing, and don't reset the variable
        cache (see next tests!)")

      (is (= '{-s-0000000000000001 (sin x)
               -s-0000000000000002 (cos y)}
             (get-tables))
          "get the replacement tables out.")

      (is (= '(+ (* 3 x) (* 2 (sin x)))
             (simplify-and-reset
              '(+ x x x (sin x) (sin x))))
          "this version resets the internal tables on each invocation.")

      (is (= '{-s-0000000000000003 (sin x)}
             (get-tables))
          "get the replacement tables out.")

      (reset-tables!)

      (is (= '{} (get-tables))
          "After an explicit reset the tables are empty again."))))
