#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.driven-pendulum-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.string]
            [clojure.test :refer [deftest is use-fixtures]]
            [emmy.env :as e :refer [up /]]
            [emmy.examples.driven-pendulum :as driven]
            [emmy.expression.analyze :as a]
            [emmy.expression.compile :refer [compile-state-fn]]
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

(deftest as-clojure
  (let [compile (fn [calling-convention]
                  (compile-state-fn driven/state-derivative
                                    '[m l g a omega]
                                    (up 't 'theta 'thetadot)
                                    {:mode :clj
                                     :gensym-fn (a/monotonic-symbol-generator 2)
                                     :calling-convention calling-convention}))]
    (is (= `(fn [[~'y01 ~'y02 ~'y03] [~'p04 ~'p05 ~'p06 ~'p07 ~'p08]]
              (let [~'_09 (~'Math/sin ~'y02)]
                (clojure.core/vector 1.0
                                     ~'y03
                                     (clojure.core//
                                      (clojure.core/+
                                       (clojure.core/* ~'p07 (~'Math/pow ~'p08 2.0) ~'_09 (~'Math/cos (clojure.core/* ~'p08 ~'y01)))
                                       (clojure.core/* -1.0 ~'p06 ~'_09))
                                      ~'p05))))
           (compile :structure)))
    (is (= `(clojure.core/fn
              [~'a09 ~'a10 ~'a11]
              (clojure.core/let
               [~'y01 (clojure.core/aget ~'a09 0)
                ~'y02 (clojure.core/aget ~'a09 1)
                ~'y03 (clojure.core/aget ~'a09 2)
                ~'p04 (clojure.core/aget ~'a11 0)
                ~'p05 (clojure.core/aget ~'a11 1)
                ~'p06 (clojure.core/aget ~'a11 2)
                ~'p07 (clojure.core/aget ~'a11 3)
                ~'p08 (clojure.core/aget ~'a11 4)
                ~'_12 (~'Math/sin ~'y02)]
                (clojure.core/doto
                 ~'a10
                  (clojure.core/aset 0 1.0)
                  (clojure.core/aset 1 ~'y03)
                  (clojure.core/aset
                   2
                   (clojure.core//
                    (clojure.core/+
                     (clojure.core/*
                      ~'p07
                      (~'Math/pow ~'p08 2.0)
                      ~'_12
                      (~'Math/cos (clojure.core/* ~'p08 ~'y01)))
                     (clojure.core/* -1.0 ~'p06 ~'_12))
                    ~'p05)))))
           (compile :primitive)))))

(deftest as-javascript
  (is (= ["[y01, y02, y03]"
          "[p04, p05, p06, p07, p08]"
          (maybe-defloatify
            (str
             "  const _09 = Math.sin(y02);\n"
             "  return [1.0, y03, (p07 * Math.pow(p08, 2.0) * _09 * Math.cos(p08 * y01) - p06 * _09) / p05];"))]
         (compile-state-fn driven/state-derivative
                            '[m l g a omega]
                            (up 't 'theta 'thetadot)
                            {:mode :js
                             :gensym-fn (a/monotonic-symbol-generator 2)})))
  (is (= ["a09" "a10" "a11"
          (maybe-defloatify
           (str
            "  const y01 = a09[0];\n"
            "  const y02 = a09[1];\n"
            "  const y03 = a09[2];\n"
            "  const p04 = a11[0];\n"
            "  const p05 = a11[1];\n"
            "  const p06 = a11[2];\n"
            "  const p07 = a11[3];\n"
            "  const p08 = a11[4];\n"
            "  const _12 = Math.sin(y02);\n"
            "  a10[0] = 1.0;\n"
            "  a10[1] = y03;\n"
            "  a10[2] = (p07 * Math.pow(p08, 2.0) * _12 * Math.cos(p08 * y01) - p06 * _12) / p05;"))]
         (compile-state-fn driven/state-derivative
                            '[m l g a omega]
                            (up 't 'theta 'thetadot)
                            {:mode :js
                             :calling-convention :primitive
                             :gensym-fn (a/monotonic-symbol-generator 2)})))

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
         (compile-state-fn
          #(e/Hamiltonian->state-derivative
            (e/Lagrangian->Hamiltonian
             (driven/L 'm 'l 'g 'a 'omega)))
          []
          (e/->H-state 't 'theta 'p_theta)
          {:mode :js
           :gensym-fn (a/monotonic-symbol-generator 2)}))))
