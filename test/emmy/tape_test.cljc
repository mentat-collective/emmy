#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.tape-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.abstract.number]
            [emmy.calculus.derivative :refer [D]]
            [emmy.generic :as g]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.tape :as t]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest sort-tests
  (is (= [1 3 2 4]
         (map t/tape-id
              (t/topological-sort-by-id
               (t/->TapeCell 0 1 0
                             {(t/->TapeCell 0 2 0 {(t/->TapeCell 0 4 0 []) 10}) 10
                              (t/->TapeCell 0 3 0 {}) 10
                              (t/->TapeCell 0 4 0 {}) 10}))))))

(deftest basic-tests
  (testing
      "simple simplification works. Everything works only with a single numeric
      output now.

      TODO handle structural outputs."
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
               ((t/gradient-r f) ['x 'y 'z])))))

      (is (= (g/simplify
              ((t/gradient-r f) ['x 'y 'z]))
             (g/simplify
              ((D f) ['x 'y 'z])))
          "reverse-mode matches forward-mode."))))
