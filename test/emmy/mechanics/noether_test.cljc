#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.noether-test
  (:require [clojure.test :refer [is deftest]]
            [emmy.abstract.function :as f]
            [emmy.generic :as g]
            [emmy.mechanics.lagrange :as l]
            [emmy.mechanics.noether :as n]
            [emmy.mechanics.rotation :as r]
            [emmy.structure :as s :refer [up]]))

(defn F-tilde [theta phi psi]
  (comp (r/Rx theta)
        (r/Ry phi)
        (r/Rz psi)
        l/coordinate))

(deftest noether-tests
  (is (= '(down (+ (* -1 m vy z) (* m vz y))
                (+ (* m vx z) (* -1 m vz x))
                (+ (* -1 m vx y) (* m vy x)))
         (g/freeze
          (g/simplify
           ((n/Noether-integral
             (l/L-central-rectangular 'm (f/literal-function 'Vr))
             F-tilde)
            (up 't
                (up 'x 'y 'z)
                (up 'vx 'vy 'vz))))))))
