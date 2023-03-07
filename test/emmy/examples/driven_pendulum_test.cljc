#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.driven-pendulum-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [emmy.env :as e :refer [up /]]
            [emmy.examples.driven-pendulum :as driven]
            [emmy.expression.compile :refer [compile-state-fn*]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(defn- gensym-fn
  []
  (let [i (atom 0)]
    (fn [x]
      (let [n (str (swap! i inc))
            n (if (= (count n) 1) (str "0" n) n)]
        (symbol (str x n))))))

(deftest equations
  (e/with-literal-functions [θ]
    (is (= '(+ (* -1 a l m (expt ω 2) (sin (θ t)) (cos (* t ω)))
               (* g l m (sin (θ t)))
               (* (expt l 2) m (((expt D 2) θ) t)))
           (e/freeze
            (e/simplify (((e/Lagrange-equations
                           (driven/L 'm 'l 'g 'a 'ω))
                          θ)
                         't)))))
    (let [o (atom [])
          observe (fn [t q] (swap! o conj [t q]))]
      (driven/evolver {:t (/ 3 60) :dt (/ 1 60) :observe observe})
      (is (= 4 (count @o))))))

(deftest as-javascript
  (is (= ["[y01, y02, y03]"
          "[p04, p05, p06, p07, p08]"
          (str
           "  const _09 = Math.sin(y02);\n"
           "  return [1.0, y03, (p07 * Math.pow(p08, 2.0) * _09 * Math.cos(p08 * y01) - p06 * _09) / p05];")]
         (compile-state-fn* driven/state-derivative
                            '[m l g a omega]
                            (up 't 'theta 'thetadot)
                            {:mode :js
                             :gensym-fn (gensym-fn)
                             :deterministic? true})))

  (is (= ["[y01, y02, y03]"
          "_"
          (str
           "  const _04 = omega * y01;\n"
           "  const _05 = Math.cos(y02);\n"
           "  const _06 = Math.pow(l, 2);\n"
           "  const _08 = Math.sin(y02);\n"
           "  const _09 = Math.sin(_04);\n"
           "  return [1.0, (a * l * m * omega * _08 * _09 + y03) / (_06 * m), (- Math.pow(a, 2.0) * l * m * Math.pow(omega, 2.0) * _08 * Math.pow(_09, 2.0) * _05 - a * omega * y03 * _09 * _05 - g * _06 * m * _08) / l];")]
         (compile-state-fn*
          #(e/Hamiltonian->state-derivative
            (e/Lagrangian->Hamiltonian
             (driven/L 'm 'l 'g 'a 'omega)))
          []
          (e/->H-state 't 'theta 'p_theta)
          {:mode :js
           :gensym-fn (gensym-fn)
           :deterministic? true}))))
