#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.central-potential-test
  (:refer-clojure :exclude [+ - * / partial])
  (:require [clojure.test :refer [deftest is]]
            [emmy.env :as e :refer [up + - * / partial]]
            [emmy.examples.central-potential :as central]
            [emmy.value :as v]))

(deftest equations
  (e/with-literal-functions
    [x y]
    (let [state (up 't (up 'x 'y 'X 'Y) (up 'dx 'dy 'dX 'dY))]
      (is (= '(+ (* (/ 1 2) (expt dX 2) m2)
                 (* (/ 1 2) (expt dY 2) m2)
                 (* (/ 1 2) (expt dx 2) m1)
                 (* (/ 1 2) (expt dy 2) m1))
             (e/freeze
              (e/simplify ((central/T 'm1 'm2) state)))))
      ;; NB: teach simplifier to recognize difference of squares below
      (is (v/= '(/ (* -1 m1 m2)
                   (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
               (e/simplify
                ((central/V 'm1 'm2) state))))

      ;; (is (println "unsimplified central" ((central/L 'm1 'm2) state)))
      (is (= '(/ (+ (* (/ 1 2) (expt dX 2) m2 (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                    (* (/ 1 2) (expt dY 2) m2 (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                    (* (/ 1 2) (expt dx 2) m1 (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                    (* (/ 1 2) (expt dy 2) m1 (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                    (* m1 m2))
                 (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
             (e/freeze
              (e/simplify ((central/L 'm1 'm2) state)))))
      (let [F ((partial 1) (central/L 'm1 'm2))
            P ((partial 2) (central/L 'm1 'm2))
            N (- F
                 (+ ((partial 0) P)
                    (* ((partial 1) P) e/velocity)))
            A ((partial 2) P)]
        (is (= '(down (/ (+ (* X m1 m2)
                            (* -1 m1 m2 x))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))))
                      (/ (+ (* Y m1 m2) (* -1 m1 m2 y))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))))
                      (/ (+ (* -1 X m1 m2)
                            (* m1 m2 x))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))))
                      (/ (+ (* -1 Y m1 m2) (* m1 m2 y))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2)))))))
               (e/freeze
                (e/simplify (F state)))))
        (is (= '(down (* dx m1)
                      (* dy m1)
                      (* dX m2)
                      (* dY m2))
               (e/freeze
                (e/simplify (P state)))))
        (is (= '(down (/ (+ (* X m1 m2) (* -1 m1 m2 x))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))))
                      (/ (+ (* Y m1 m2) (* -1 m1 m2 y))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))))
                      (/ (+ (* -1 X m1 m2) (* m1 m2 x))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))))
                      (/ (+ (* -1 Y m1 m2) (* m1 m2 y))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2)))))))
               (e/freeze
                (e/simplify (N state)))))

        (is (= '(down 0 0 0 0)
               (e/freeze
                (e/simplify ((- F N) state)))))

        (is (= 4 (count (A state))))
        (is (= [4 4 4 4] (map count (A state))))
        (is (= '(down (down m1 0 0 0)
                      (down 0 m1 0 0)
                      (down 0 0 m2 0)
                      (down 0 0 0 m2))
               (e/freeze
                (e/simplify (A state)))))

        (is (= '(up (up (/ 1 m1) 0 0 0)
                    (up 0 (/ 1 m1) 0 0)
                    (up 0 0 (/ 1 m2) 0)
                    (up 0 0 0 (/ 1 m2)))
               (e/freeze
                (e/simplify (/ (A state)))))))

      (is (= '(down (down (* m (((expt D 2) x) t))
                          (* m (((expt D 2) y) t)))
                    (down 0 0))
             (e/freeze
              (e/simplify (((e/Lagrange-equations (central/L 'm))
                            (up (up x y)
                                (up (constantly 0) (constantly 0))))
                           't)))))
      (is (= '(up 1
                  (up dx dy dX dY)
                  (up (/ (+ (* M X) (* -1 M x))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))))
                      (/ (+ (* M Y) (* -1 M y))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))))
                      (/ (+ (* -1 X m) (* m x))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))))
                      (/ (+ (* -1 Y m) (* m y))
                         (+ (* (expt X 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 X x (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt Y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* -2 Y y (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt x 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))
                            (* (expt y 2) (sqrt (+ (expt X 2) (* -2 X x) (expt Y 2) (* -2 Y y) (expt x 2) (expt y 2))))))))
             (e/freeze
              (e/simplify
               ((central/state-derivative 'm 'M) state)))))
      (is (= 4 (count
                (central/evolver
                 {:t (/ 3 60) :dt (/ 1 60)})))))))
