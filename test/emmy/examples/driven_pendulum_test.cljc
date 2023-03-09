#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.driven-pendulum-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.string]
            [clojure.test :refer [deftest is use-fixtures]]
            [emmy.env :as e :refer [up /]]
            [emmy.examples.driven-pendulum :as driven]
            [emmy.expression.analyze :as a]
            [emmy.expression.compile :refer [compile-state-fn*]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(defn- maybe-defloatify
  "Clojure renders integer doubles like 1.0, but Clojurescript like 1.
   Our expected values come from the reference Clojure implementation,
   and this function will convert to the Clojurescript expected form."
  [s]
  #?(:cljs (clojure.string/replace s #"\b(\d+)\.0\b" (fn [[_ m]] m))
     :clj s))

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
          (maybe-defloatify
            (str
             "  const _09 = Math.sin(y02);\n"
             "  return [1.0, y03, (p07 * Math.pow(p08, 2.0) * _09 * Math.cos(p08 * y01) - p06 * _09) / p05];"))]
         (compile-state-fn* driven/state-derivative
                            '[m l g a omega]
                            (up 't 'theta 'thetadot)
                            {:mode :js
                             :gensym-fn (a/monotonic-symbol-generator 2)
                             :deterministic? true})))

  (is (= ["[y01, y02, y03]"
          "_"
          (maybe-defloatify
            (str
             "  const _04 = omega * y01;\n"
             "  const _05 = Math.cos(y02);\n"
             "  const _06 = Math.pow(l, 2.0);\n"
             "  const _08 = Math.sin(y02);\n"
             "  const _09 = Math.sin(_04);\n"
             "  return [1.0, (a * l * m * omega * _08 * _09 + y03) / (_06 * m), (- Math.pow(a, 2.0) * l * m * Math.pow(omega, 2.0) * _08 * Math.pow(_09, 2.0) * _05 - a * omega * y03 * _09 * _05 - g * _06 * m * _08) / l];"))]
         (compile-state-fn*
          #(e/Hamiltonian->state-derivative
            (e/Lagrangian->Hamiltonian
             (driven/L 'm 'l 'g 'a 'omega)))
          []
          (e/->H-state 't 'theta 'p_theta)
          {:mode :js
           :gensym-fn (a/monotonic-symbol-generator 2)
           :deterministic? true}))))
