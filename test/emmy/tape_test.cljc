#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.tape-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.abstract.number]
            [emmy.calculus.derivative :refer [D]]
            [emmy.generic :as g]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.tape :as t]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest basic-tests
  (testing
      "simple simplification works. Everything works only with a single numeric
      output now. TODO handle structural outputs."
    (let [f (fn [[x y z]]
              (g/+ (g/expt x 4) (g/* x y z (g/cos x))))]
      (is (= '(down
               (+ (* -1 x y z (sin x))
                  (* 4 (expt x 3))
                  (* y z (cos x)))
               (* x z (cos x))
               (* x y (cos x)))
             (v/freeze
              (g/simplify
               ((t/gradient f) ['x 'y 'z])))))

      (is (= (g/simplify
              ((t/gradient f) ['x 'y 'z]))
             (g/simplify
              ((D f) ['x 'y 'z])))
          "reverse-mode matches forward-mode."))))
