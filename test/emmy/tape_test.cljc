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
  (let [x (t/make 0 'x)
        y (t/make 0 'y)
        a (g/mul x y)
        b (g/sin x)
        z (g/add a b)]
    (is (= [z b a y x]
           (t/topological-sort z)))))

(deftest basic-tests
  (testing
      "simple simplification works. Everything works only with a single numeric
      output now."
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
              ((t/gradient f) ['x 'y 'z]))
             (g/simplify
              ((D f) ['x 'y 'z])))
          "reverse-mode matches forward-mode."))

    (let [f (fn [a b c d e f]
              [(g/* (g/cos a) (g/cos b))
               (g/* (g/cos c) (g/cos d))
               (g/* (g/cos e) (g/cos f))])]
      (is (=
           (g/simplify
            ((t/gradient (t/gradient f)) 'a 'b 'c 'd 'e 'f))
           (g/simplify
            ((D (D f)) 'a 'b 'c 'd 'e 'f)))
          "multivariable derivatives match"))))
