#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.sicm.ch3-test
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.env :as e
             :refer [+ zero? up down literal-function]]
            [emmy.examples.driven-pendulum :as driven]
            [emmy.examples.top :as top]
            [emmy.expression.compile :as c]
            [emmy.mechanics.hamilton :as H]
            [emmy.mechanics.lagrange :as L]
            [emmy.polynomial.gcd :as pg]
            [emmy.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp e/freeze e/simplify))

(defn- gensym-fn
  []
  (let [i (atom 0)]
    (fn [x]
      (let [n (str (swap! i inc))
            n (if (= (count n) 1) (str "0" n) n)]
        (symbol (str x n))))))

(deftest section-3-1
  (testing "p.189"
    (is (= '(up 0
                (up (/ (+ (* m ((D x) t)) (* -1 (p_x t))) m)
                    (/ (+ (* m ((D y) t)) (* -1 (p_y t))) m))
                (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t)))
                      (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))
           (simplify
            (((e/Hamilton-equations
               (H/H-rectangular
                'm
                (literal-function 'V (-> (X Real Real) Real))))
              (up (literal-function 'x) (literal-function 'y))
              (down (literal-function 'p_x) (literal-function 'p_y)))
             't)))))
  (testing "p.198"
    (is (= '(/ (+ (* m (V x y))
                  (* (/ 1 2) (expt p_x 2))
                  (* (/ 1 2) (expt p_y 2)))
               m)
           (simplify
            ((e/Lagrangian->Hamiltonian
              (L/L-rectangular
               'm (literal-function 'V (-> (X Real Real) Real))))
             (up 't (up 'x 'y) (down 'p_x 'p_y))))))))

(deftest section-3-2
  (testing "p.205"
    (let [F (literal-function 'F (H/Hamiltonian 2))
          G (literal-function 'G (H/Hamiltonian 2))
          H (literal-function 'H (H/Hamiltonian 2))]
      (is (zero?
           (simplify
            ((+ (e/Poisson-bracket F (e/Poisson-bracket G H))
                (e/Poisson-bracket G (e/Poisson-bracket H F))
                (e/Poisson-bracket H (e/Poisson-bracket F G)))
             (up 't (up 'x 'y) (down 'px 'py)))))))))

(deftest section-3-4
  (testing "p.212"
    (is (= '(/ (+ (* m (expt r 2) (V r))
                  (* (/ 1 2) (expt p_r 2) (expt r 2))
                  (* (/ 1 2) (expt p_phi 2)))
               (* m (expt r 2)))
           (simplify
            ((e/Lagrangian->Hamiltonian
              (L/L-central-polar 'm (literal-function 'V)))
             (up 't (up 'r 'phi) (down 'p_r 'p_phi))))))

    (is (= '(up 0
                (up (/ (+ (* m ((D r) t))
                          (* -1 (p_r t)))
                       m)
                    (/ (+ (* m (expt (r t) 2) ((D phi) t))
                          (* -1 (p_phi t)))
                       (* m (expt (r t) 2))))
                (down (/ (+ (* m (expt (r t) 3) ((D p_r) t))
                            (* m (expt (r t) 3) ((D V) (r t)))
                            (* -1 (expt (p_phi t) 2)))
                         (* m (expt (r t) 3)))
                      ((D p_phi) t)))
           (simplify
            (((e/Hamilton-equations
               (e/Lagrangian->Hamiltonian
                (L/L-central-polar 'm (literal-function 'V))))
              (up (literal-function 'r)
                  (literal-function 'phi))
              (down (literal-function 'p_r)
                    (literal-function 'p_phi)))
             't)))))

  (testing "p.213"
    (is (= '(/ (+ (* A C gMR (expt (sin theta) 2) (cos theta))
                  (* (/ 1 2) A (expt p_psi 2) (expt (sin theta) 2))
                  (* (/ 1 2) C (expt p_psi 2) (expt (cos theta) 2))
                  (* (/ 1 2) C (expt p_theta 2) (expt (sin theta) 2))
                  (* -1 C p_phi p_psi (cos theta))
                  (* (/ 1 2) C (expt p_phi 2)))
               (* A C (expt (sin theta) 2)))
           (simplify
            ((e/Lagrangian->Hamiltonian
              (top/L-axisymmetric 'A 'C 'gMR))
             (up 't
                 (up 'theta 'phi 'psi)
                 (down 'p_theta 'p_phi 'p_psi)))))))

  (testing "p.214"
    (let [top-state (up 't
                        (up 'theta 'phi 'psi)
                        (down 'p_theta 'p_phi 'p_psi))
          H (e/Lagrangian->Hamiltonian
             (top/L-axisymmetric 'A 'C 'gMR))
          sysder (e/Hamiltonian->state-derivative H)]
      (is (= '(/ (+ (* A C gMR (expt (sin theta) 2) (cos theta))
                    (* (/ 1 2) A (expt p_psi 2) (expt (sin theta) 2))
                    (* (/ 1 2) C (expt p_psi 2) (expt (cos theta) 2))
                    (* (/ 1 2) C (expt p_theta 2) (expt (sin theta) 2))
                    (* -1 C p_phi p_psi (cos theta))
                    (* (/ 1 2) C (expt p_phi 2)))
                 (* A C (expt (sin theta) 2)))
             (simplify (H top-state))))

      ;; This was giving cljs some trouble on CI, so here we are.
      (binding [pg/*poly-gcd-time-limit* #?(:clj  [2 :seconds]
                                            :cljs [6 :seconds])]
        (is (= '(up 1
                    (up (/ p_theta A)
                        (/ (+ (* -1 p_psi (cos theta)) p_phi) (* A (expt (sin theta) 2)))
                        (/ (+ (* A p_psi (expt (sin theta) 2)) (* C p_psi (expt (cos theta) 2)) (* -1 C p_phi (cos theta)))
                           (* A C (expt (sin theta) 2))))
                    (down (/ (+ (* A gMR (expt (cos theta) 4))
                                (* -2 A gMR (expt (cos theta) 2))
                                (* -1 p_phi p_psi (expt (cos theta) 2))
                                (* (expt p_phi 2) (cos theta))
                                (* (expt p_psi 2) (cos theta))
                                (* A gMR)
                                (* -1 p_phi p_psi))
                             (* A (expt (sin theta) 3)))
                          0
                          0))
               (simplify (sysder top-state)))))

      (is (= ["[y01, y02, y03, y04, y05, y06, y07]"
                "_"
                (str
                 "  const _08 = Math.sin(y02);\n"
                 "  const _09 = Math.cos(y02);\n"
                 "  const _12 = Math.pow(_08, 2);\n"
                 "  const _13 = Math.pow(_09, 2);\n"
                 "  return [1.0, [y05 / A, (- y07 * _09 + y06) / (A * _12), (A * y07 * _12 + C * y07 * _13 - C * y06 * _09) / (A * C * _12)], [(A * gMR * Math.pow(_09, 4.0) + -2.0 * A * gMR * _13 - y06 * y07 * _13 + Math.pow(y06, 2.0) * _09 + Math.pow(y07, 2.0) * _09 + A * gMR - y06 * y07) / (A * Math.pow(_08, 3.0)), 0.0, 0.0]];")]
                 (c/compile-state-fn* (fn [] sysder) [] top-state {:mode :js
                                                                   :gensym-fn (gensym-fn)})))))

  (deftest section-3-5
    (testing "p.221"
      (let [H ((e/Lagrangian->Hamiltonian
                (driven/L 'm 'l 'g 'a 'omega))
               (up 't 'theta 'p_theta))]
        (is (= '(/ (+ (* (/ -1 2)
                         (expt a 2) (expt l 2) (expt m 2)
                         (expt omega 2)
                         (expt (sin (* omega t)) 2)
                         (expt (cos theta) 2))
                      (* a g (expt l 2) (expt m 2) (cos (* omega t)))
                      (* a l m omega p_theta (sin (* omega t)) (sin theta))
                      (* -1 g (expt l 3) (expt m 2) (cos theta))
                      (* (/ 1 2) (expt p_theta 2)))
                   (* (expt l 2) m))
               (simplify H))))
      (let [sysder (simplify
                    ((e/Hamiltonian->state-derivative
                      (e/Lagrangian->Hamiltonian
                       (driven/L 'm 'l 'g 'a 'omega)))
                     (up 't 'theta 'p_theta)))]
        (is (= '(up 1
                    (/ (+ (* a l m omega (sin (* omega t)) (sin theta)) p_theta)
                       (* (expt l 2) m))
                    (/ (+ (* -1 (expt a 2) l m (expt omega 2) (expt (sin (* omega t)) 2) (sin theta) (cos theta))
                          (* -1 a omega p_theta (sin (* omega t)) (cos theta))
                          (* -1 g (expt l 2) m (sin theta)))
                       l))
               sysder))

        (is (= ["[y01, y02, y03]"
                "_"
                (str
                 "  const _04 = Math.sin(y02);\n"
                 "  const _05 = Math.cos(y02);\n"
                 "  const _06 = Math.pow(l, 2);\n"
                 "  const _08 = omega * y01;\n"
                 "  const _09 = Math.sin(_08);\n"
                 "  return [1.0, (a * l * m * omega * _09 * _04 + y03) / (_06 * m), (- Math.pow(a, 2.0) * l * m * Math.pow(omega, 2.0) * Math.pow(_09, 2.0) * _04 * _05 - a * omega * y03 * _09 * _05 - g * _06 * m * _04) / l];")]
               (c/compile-state-fn*
                (fn []
                  (e/Hamiltonian->state-derivative
                   (e/Lagrangian->Hamiltonian
                    (driven/L 'm 'l 'g 'a 'omega))))
                []
                (up 't 'theta 'p_theta)
                {:mode :js
                 :gensym-fn (gensym-fn)})))))))
