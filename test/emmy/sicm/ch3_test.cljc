#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.sicm.ch3-test
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [clojure.string :as s]
            [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.env :as e
             :refer [+ zero? up down literal-function]]
            [emmy.examples.driven-pendulum :as driven]
            [emmy.examples.top :as top]
            [emmy.expression.analyze :as a]
            [emmy.expression.compile :as c]
            [emmy.mechanics.hamilton :as H]
            [emmy.mechanics.lagrange :as L]
            [emmy.polynomial.gcd :as pg]
            [emmy.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(defn- maybe-defloatify
  "Clojure renders integer doubles like 1.0, but Clojurescript like 1.
   Our expected values come from the reference Clojure implementation,
   and this function will convert to the Clojurescript expected form."
  [s]
  #?(:cljs (s/replace s #"\b(\d+)\.0\b" (fn [[_ m]] m))
     :clj s))

(def simplify
  (comp e/freeze e/simplify))

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

      (is (= ["[y01, [y02, y03, y04], [y05, y06, y07]]"
              "_"
              (maybe-defloatify
               (s/join "\n" ["  const _08 = 1.0;"
                             "  const _09 = y05 / A;"
                             "  const _10 = -1.0;"
                             "  const _11 = _10 * y07;"
                             "  const _12 = Math.cos(y02);"
                             "  const _13 = _11 * _12;"
                             "  const _14 = _13 + y06;"
                             "  const _15 = Math.sin(y02);"
                             "  const _16 = 2.0;"
                             "  const _17 = Math.pow(_15, _16);"
                             "  const _18 = A * _17;"
                             "  const _19 = _14 / _18;"
                             "  const _20 = A * y07;"
                             "  const _21 = _20 * _17;"
                             "  const _22 = C * y07;"
                             "  const _23 = Math.pow(_12, _16);"
                             "  const _24 = _22 * _23;"
                             "  const _25 = _21 + _24;"
                             "  const _26 = _10 * C;"
                             "  const _27 = _26 * y06;"
                             "  const _28 = _27 * _12;"
                             "  const _29 = _25 + _28;"
                             "  const _30 = A * C;"
                             "  const _31 = _30 * _17;"
                             "  const _32 = _29 / _31;"
                             "  const _33 = [_09, _19, _32];"
                             "  const _34 = A * gMR;"
                             "  const _35 = 4.0;"
                             "  const _36 = Math.pow(_12, _35);"
                             "  const _37 = _34 * _36;"
                             "  const _38 = -2.0;"
                             "  const _39 = _38 * A;"
                             "  const _40 = _39 * gMR;"
                             "  const _41 = _40 * _23;"
                             "  const _42 = _37 + _41;"
                             "  const _43 = _10 * y06;"
                             "  const _44 = _43 * y07;"
                             "  const _45 = _44 * _23;"
                             "  const _46 = _42 + _45;"
                             "  const _47 = Math.pow(y06, _16);"
                             "  const _48 = _47 * _12;"
                             "  const _49 = _46 + _48;"
                             "  const _50 = Math.pow(y07, _16);"
                             "  const _51 = _50 * _12;"
                             "  const _52 = _49 + _51;"
                             "  const _53 = _52 + _34;"
                             "  const _54 = _53 + _44;"
                             "  const _55 = 3.0;"
                             "  const _56 = Math.pow(_15, _55);"
                             "  const _57 = A * _56;"
                             "  const _58 = _54 / _57;"
                             "  const _59 = 0.0;"
                             "  const _60 = [_58, _59, _59];"
                             "  const _61 = [_08, _33, _60];"
                             "  return _61;"]))]
             (c/compile-state-fn (fn [] sysder) [] top-state
               {:mode :js
                :gensym-fn (a/monotonic-symbol-generator 2)})))))

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
                (maybe-defloatify
                 (s/join "\n" ["  const _04 = 1.0;"
                               "  const _05 = a * l;"
                               "  const _06 = _05 * m;"
                               "  const _07 = _06 * omega;"
                               "  const _08 = omega * y01;"
                               "  const _09 = Math.sin(_08);"
                               "  const _10 = _07 * _09;"
                               "  const _11 = Math.sin(y02);"
                               "  const _12 = _10 * _11;"
                               "  const _13 = _12 + y03;"
                               "  const _14 = 2.0;"
                               "  const _15 = Math.pow(l, _14);"
                               "  const _16 = _15 * m;"
                               "  const _17 = _13 / _16;"
                               "  const _18 = -1.0;"
                               "  const _19 = Math.pow(a, _14);"
                               "  const _20 = _18 * _19;"
                               "  const _21 = _20 * l;"
                               "  const _22 = _21 * m;"
                               "  const _23 = Math.pow(omega, _14);"
                               "  const _24 = _22 * _23;"
                               "  const _25 = Math.pow(_09, _14);"
                               "  const _26 = _24 * _25;"
                               "  const _27 = _26 * _11;"
                               "  const _28 = Math.cos(y02);"
                               "  const _29 = _27 * _28;"
                               "  const _30 = _18 * a;"
                               "  const _31 = _30 * omega;"
                               "  const _32 = _31 * y03;"
                               "  const _33 = _32 * _09;"
                               "  const _34 = _33 * _28;"
                               "  const _35 = _29 + _34;"
                               "  const _36 = _18 * g;"
                               "  const _37 = _36 * _15;"
                               "  const _38 = _37 * m;"
                               "  const _39 = _38 * _11;"
                               "  const _40 = _35 + _39;"
                               "  const _41 = _40 / l;"
                               "  const _42 = [_04, _17, _41];"
                               "  return _42;"]))]
               (c/compile-state-fn
                (fn []
                  (e/Hamiltonian->state-derivative
                   (e/Lagrangian->Hamiltonian
                    (driven/L 'm 'l 'g 'a 'omega))))
                []
                (up 't 'theta 'p_theta)
                {:mode :js
                 :gensym-fn (a/monotonic-symbol-generator 2)})))))))
