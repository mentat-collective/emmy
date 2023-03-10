#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.double-pendulum-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.string]
            [clojure.test :refer [is deftest use-fixtures]]
            [emmy.env :as e :refer [up down /]]
            [emmy.examples.double-pendulum :as double]
            [emmy.expression.analyze :as a]
            [emmy.expression.compile :as c]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest equations
  (let [state (up 't (up 'θ 'φ) (up 'θdot 'φdot))
        V (double/V 'm1 'm2 'l1 'l2 'g)
        T (double/T 'm1 'm2 'l1 'l2 'g)
        L (double/L 'm1 'm2 'l1 'l2 'g)]
    (is (= '(+ (* -1 g l1 m1 (cos θ))
               (* -1 g l1 m2 (cos θ))
               (* -1 g l2 m2 (cos φ)))
           (v/freeze
            (e/simplify (V state)))))

    (is (= '(+ (* l1 l2 m2 θdot φdot (cos (+ θ (* -1 φ))))

               (* (/ 1 2) (expt l1 2) m1 (expt θdot 2))
               (* (/ 1 2) (expt l1 2) m2 (expt θdot 2))
               (* (/ 1 2) (expt l2 2) m2 (expt φdot 2)))
           (v/freeze
            (e/simplify (T state)))))
    (is (= '(+ (* l1 l2 m2 θdot φdot (cos (+ θ (* -1 φ))))
               (* (/ 1 2) (expt l1 2) m1 (expt θdot 2))
               (* (/ 1 2) (expt l1 2) m2 (expt θdot 2))
               (* (/ 1 2) (expt l2 2) m2 (expt φdot 2))
               (* g l1 m1 (cos θ))
               (* g l1 m2 (cos θ))
               (* g l2 m2 (cos φ)))
           (v/freeze
            (e/simplify (L state)))))

    (e/with-literal-functions [θ φ]
      (is (= '(down
               (+ (* l1 l2 m2 (sin (+ (θ t) (* -1 (φ t)))) (expt ((D φ) t) 2))
                  (* l1 l2 m2 (cos (+ (θ t) (* -1 (φ t)))) (((expt D 2) φ) t))
                  (* g l1 m1 (sin (θ t)))
                  (* g l1 m2 (sin (θ t)))
                  (* (expt l1 2) m1 (((expt D 2) θ) t))
                  (* (expt l1 2) m2 (((expt D 2) θ) t)))
               (+ (* -1 l1 l2 m2 (sin (+ (θ t) (* -1 (φ t)))) (expt ((D θ) t) 2))
                  (* l1 l2 m2 (cos (+ (θ t) (* -1 (φ t)))) (((expt D 2) θ) t))
                  (* g l2 m2 (sin (φ t)))
                  (* (expt l2 2) m2 (((expt D 2) φ) t))))
             (v/freeze
              (e/simplify (((e/Lagrange-equations
                             (double/L 'm1 'm2 'l1 'l2 'g))
                            (up θ φ))
                           't))))))
    (let [o (atom [])
          observe (fn [t q] (swap! o conj [t q]))]
      (double/evolver {:t (/ 3 60) :dt (/ 1 60) :observe observe})
      (is (= 4 (count @o))))))

(defn- maybe-defloatify
  "Clojure renders integer doubles like 1.0, but Clojurescript like 1.
   Our expected values come from the reference Clojure implementation,
   and this function will convert to the Clojurescript expected form."
  [s]
  #?(:cljs (clojure.string/replace s #"\b(\d+)\.0\b" (fn [[_ m]] m))
     :clj s))

(deftest infix-forms
  (is (=
       ["[y01, [y02, y03], [y04, y05]]"
        (maybe-defloatify
         (str
          "  const _06 = - y03;\n"
          "  const _10 = Math.pow(y04, 2.0);\n"
          "  const _11 = Math.pow(y05, 2.0);\n"
          "  const _13 = Math.sin(y02);\n"
          "  const _14 = Math.sin(y03);\n"
          "  const _15 = y02 + _06;\n"
          "  const _19 = Math.cos(_15);\n"
          "  const _21 = Math.sin(_15);\n"
          "  const _22 = Math.pow(_21, 2.0);\n"
          "  return [1.0, [y04, y05], [(- l1 * m2 * _10 * _19 * _21 - l2 * m2 * _11 * _21 + g * m2 * _19 * _14 - g * m1 * _13 - g * m2 * _13) / (l1 * m2 * _22 + l1 * m1), (l2 * m2 * _11 * _19 * _21 + l1 * m1 * _10 * _21 + l1 * m2 * _10 * _21 + g * m1 * _19 * _13 + g * m2 * _19 * _13 - g * m1 * _14 - g * m2 * _14) / (l2 * m2 * _22 + l2 * m1)]];"))]
       (c/compile-state-fn* double/state-derivative
                            '[m1 m2 l1 l2 g]
                            (up 't (up 'theta 'phi) (up 'thetadot 'phidot))
                            {:mode :js
                             :flatten? false
                             :generic-params? false
                             :gensym-fn (a/monotonic-symbol-generator 2)
                             :deterministic? true})))


  (is (= ["[y01, y02, y03, y04, y05]"
          "[p06, p07, p08, p09, p10]"
          (maybe-defloatify
           (str
            "  const _11 = - y03;\n"
            "  const _15 = Math.pow(y04, 2.0);\n"
            "  const _16 = Math.pow(y05, 2.0);\n"
            "  const _18 = Math.sin(y02);\n"
            "  const _19 = Math.sin(y03);\n"
            "  const _20 = y02 + _11;\n"
            "  const _24 = Math.cos(_20);\n"
            "  const _26 = Math.sin(_20);\n"
            "  const _27 = Math.pow(_26, 2.0);\n"
            "  return [1.0, [y04, y05], [(- p07 * p08 * _15 * _24 * _26 - p07 * p09 * _16 * _26 + p07 * p10 * _24 * _19 - p06 * p10 * _18 - p07 * p10 * _18) / (p07 * p08 * _27 + p06 * p08), (p07 * p09 * _16 * _24 * _26 + p06 * p08 * _15 * _26 + p07 * p08 * _15 * _26 + p06 * p10 * _24 * _18 + p07 * p10 * _24 * _18 - p06 * p10 * _19 - p07 * p10 * _19) / (p07 * p09 * _27 + p06 * p09)]];"))]
         (c/compile-state-fn*
          double/state-derivative
          '[1 1 1 1 'g]
          (up 't (up 'theta 'phi) (up 'thetadot 'phidot))
          {:mode :js
           :gensym-fn (a/monotonic-symbol-generator 2)
           :deterministic? true})))

  (let [eq (e/simplify
            ((e/Hamiltonian->state-derivative
              (e/Lagrangian->Hamiltonian
               (double/L 'm_1 'm_2 'l_1 'l_2 'g)))
             (e/->H-state 't (up 'theta 'psi) (down 'p_theta 'p_psi))))]
    (is (= "up(1, up((- l₁ p_ψ cos(ψ - θ) + l₂ p_θ) / (l₁² l₂ m₂ sin²(ψ - θ) + l₁² l₂ m₁), (- l₂ m₂ p_θ cos(ψ - θ) + l₁ m₁ p_ψ + l₁ m₂ p_ψ) / (l₁ l₂² m₂² sin²(ψ - θ) + l₁ l₂² m₁ m₂)), down((- g l₁³ l₂² m₁ m₂² (cos(ψ - θ))⁴ sin(θ) - g l₁³ l₂² m₂³ (cos(ψ - θ))⁴ sin(θ) + 2 g l₁³ l₂² m₁² m₂ cos²(ψ - θ) sin(θ) + 4 g l₁³ l₂² m₁ m₂² cos²(ψ - θ) sin(θ) + 2 g l₁³ l₂² m₂³ cos²(ψ - θ) sin(θ) - g l₁³ l₂² m₁³ sin(θ) - 3 g l₁³ l₂² m₁² m₂ sin(θ) - 3 g l₁³ l₂² m₁ m₂² sin(θ) - g l₁³ l₂² m₂³ sin(θ) + l₁ l₂ m₂ p_ψ p_θ cos²(ψ - θ) sin(ψ - θ) - l₁² m₁ p_ψ² cos(ψ - θ) sin(ψ - θ) - l₁² m₂ p_ψ² cos(ψ - θ) sin(ψ - θ) - l₂² m₂ p_θ² cos(ψ - θ) sin(ψ - θ) + l₁ l₂ m₁ p_ψ p_θ sin(ψ - θ) + l₁ l₂ m₂ p_ψ p_θ sin(ψ - θ)) / (l₁² l₂² m₂² (cos(ψ - θ))⁴ + 2 l₁² l₂² m₁ m₂ sin²(ψ - θ) - 2 l₁² l₂² m₂² cos²(ψ - θ) + l₁² l₂² m₁² + l₁² l₂² m₂²), (- g l₁² l₂³ m₂³ (cos(ψ - θ))⁴ sin(ψ) - 2 g l₁² l₂³ m₁ m₂² sin²(ψ - θ) sin(ψ) + 2 g l₁² l₂³ m₂³ cos²(ψ - θ) sin(ψ) - g l₁² l₂³ m₁² m₂ sin(ψ) - g l₁² l₂³ m₂³ sin(ψ) - l₁ l₂ m₂ p_ψ p_θ cos²(ψ - θ) sin(ψ - θ) + l₁² m₁ p_ψ² cos(ψ - θ) sin(ψ - θ) + l₁² m₂ p_ψ² cos(ψ - θ) sin(ψ - θ) + l₂² m₂ p_θ² cos(ψ - θ) sin(ψ - θ) - l₁ l₂ m₁ p_ψ p_θ sin(ψ - θ) - l₁ l₂ m₂ p_ψ p_θ sin(ψ - θ)) / (l₁² l₂² m₂² (cos(ψ - θ))⁴ + 2 l₁² l₂² m₁ m₂ sin²(ψ - θ) - 2 l₁² l₂² m₂² cos²(ψ - θ) + l₁² l₂² m₁² + l₁² l₂² m₂²)))"
           (e/->infix eq)))

    #?(:clj
       ;; even with the deterministic flag, this is not quite reproducing in
       ;; ClojureScript.
       (is (= ["[y01, y02, y03, y04, y05]"
               "_"
               (str
                "  const _09 = - y03;\n"
                "  const _18 = Math.pow(l_1, 2.0);\n"
                "  const _19 = Math.pow(l_1, 3.0);\n"
                "  const _20 = Math.pow(l_2, 2.0);\n"
                "  const _21 = Math.pow(l_2, 3.0);\n"
                "  const _22 = Math.pow(m_1, 2.0);\n"
                "  const _23 = Math.pow(m_2, 2.0);\n"
                "  const _24 = Math.pow(m_2, 3.0);\n"
                "  const _25 = Math.pow(y04, 2.0);\n"
                "  const _26 = Math.pow(y05, 2.0);\n"
                "  const _28 = Math.sin(y02);\n"
                "  const _29 = Math.sin(y03);\n"
                "  const _32 = _18 * _20 * _22;\n"
                "  const _34 = _18 * _20 * _23;\n"
                "  const _36 = y02 + _09;\n"
                "  const _46 = Math.cos(_36);\n"
                "  const _50 = Math.sin(_36);\n"
                "  const _55 = Math.pow(_46, 2.0);\n"
                "  const _56 = Math.pow(_46, 4.0);\n"
                "  const _57 = Math.pow(_50, 2.0);\n"
                "  const _58 = - 2.0 * _18 * _20 * _23 * _55;\n"
                "  const _59 = 2.0 * _18 * _20 * m_1 * m_2 * _57;\n"
                "  const _60 = _18 * _20 * _23 * _56;\n"
                "  const _62 = _60 + _59 + _58 + _32 + _34;\n"
                "  return [1.0, [(- l_1 * y05 * _46 + l_2 * y04) / (_18 * l_2 * m_2 * _57 + _18 * l_2 * m_1), (- l_2 * m_2 * y04 * _46 + l_1 * m_1 * y05 + l_1 * m_2 * y05) / (l_1 * _20 * _23 * _57 + l_1 * _20 * m_1 * m_2)], [(- g * _19 * _20 * m_1 * _23 * _56 * _28 - g * _19 * _20 * _24 * _56 * _28 + 2.0 * g * _19 * _20 * _22 * m_2 * _55 * _28 + 4.0 * g * _19 * _20 * m_1 * _23 * _55 * _28 + 2.0 * g * _19 * _20 * _24 * _55 * _28 - g * _19 * _20 * Math.pow(m_1, 3.0) * _28 - 3.0 * g * _19 * _20 * _22 * m_2 * _28 - 3.0 * g * _19 * _20 * m_1 * _23 * _28 - g * _19 * _20 * _24 * _28 - l_1 * l_2 * m_2 * y04 * y05 * _55 * _50 + _18 * m_1 * _26 * _46 * _50 + _18 * m_2 * _26 * _46 * _50 + _20 * m_2 * _25 * _46 * _50 - l_1 * l_2 * m_1 * y04 * y05 * _50 - l_1 * l_2 * m_2 * y04 * y05 * _50) / _62, (- g * _18 * _21 * _24 * _56 * _29 - 2.0 * g * _18 * _21 * m_1 * _23 * _57 * _29 + 2.0 * g * _18 * _21 * _24 * _55 * _29 - g * _18 * _21 * _22 * m_2 * _29 - g * _18 * _21 * _24 * _29 + l_1 * l_2 * m_2 * y04 * y05 * _55 * _50 - _18 * m_1 * _26 * _46 * _50 - _18 * m_2 * _26 * _46 * _50 - _20 * m_2 * _25 * _46 * _50 + l_1 * l_2 * m_1 * y04 * y05 * _50 + l_1 * l_2 * m_2 * y04 * y05 * _50) / _62]];")]
              (c/compile-state-fn*
               #(e/Hamiltonian->state-derivative
                 (e/Lagrangian->Hamiltonian
                  (double/L 'm_1 'm_2 'l_1 'l_2 'g)))
               []
               (e/->H-state 't (up 'theta 'psi) (down 'p_theta 'p_psi))
               {:mode :js
                :gensym-fn (a/monotonic-symbol-generator 2)
                :deterministic? true}))))))
