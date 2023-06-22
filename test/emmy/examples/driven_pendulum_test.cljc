#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.driven-pendulum-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.string :as s]
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
    (is (= '(clojure.core/fn [[y01 y02 y03] [p04 p05 p06 p07 p08]]
              (clojure.core/let [_09 1.0
                                 _10 2.0
                                 _11 (Math/pow p08 _10)
                                 _12 (clojure.core/* p07 _11)
                                 _13 (Math/sin y02)
                                 _14 (clojure.core/* _12 _13)
                                 _15 (clojure.core/* p08 y01)
                                 _16 (Math/cos _15)
                                 _17 (clojure.core/* _14 _16)
                                 _18 -1.0
                                 _19 (clojure.core/* _18 p06)
                                 _20 (clojure.core/* _19 _13)
                                 _21 (clojure.core/+ _17 _20)
                                 _22 (clojure.core// _21 p05)
                                 _23 (clojure.core/vector _09 y03 _22)]
                _23))
           (compile :structure)))
    (is (= '(clojure.core/fn [a09 a10 a11]
              (clojure.core/let [y01 (clojure.core/aget a09 0)
                y02 (clojure.core/aget a09 1)
                y03 (clojure.core/aget a09 2)
                p04 (clojure.core/aget a11 0)
                p05 (clojure.core/aget a11 1)
                p06 (clojure.core/aget a11 2)
                p07 (clojure.core/aget a11 3)
                p08 (clojure.core/aget a11 4)
                _12 1.0
                _13 2.0
                _14 (Math/pow p08 _13)
                _15 (clojure.core/* p07 _14)
                _16 (Math/sin y02)
                _17 (clojure.core/* _15 _16)
                _18 (clojure.core/* p08 y01)
                _19 (Math/cos _18)
                _20 (clojure.core/* _17 _19)
                _21 -1.0
                _22 (clojure.core/* _21 p06)
                _23 (clojure.core/* _22 _16)
                _24 (clojure.core/+ _20 _23)
                _25 (clojure.core// _24 p05)]
                (clojure.core/doto a10
                  (clojure.core/aset 0 _12)
                  (clojure.core/aset 1 y03)
                  (clojure.core/aset 2 _25))))
           (compile :primitive)))))

(deftest as-javascript
  (is (= ["[y01, y02, y03]"
          "[p04, p05, p06, p07, p08]"
          (maybe-defloatify

           (s/join "\n" ["  const _09 = 1.0;"
                         "  const _10 = 2.0;"
                         "  const _11 = Math.pow(p08, _10);"
                         "  const _12 = p07 * _11;"
                         "  const _13 = Math.sin(y02);"
                         "  const _14 = _12 * _13;"
                         "  const _15 = p08 * y01;"
                         "  const _16 = Math.cos(_15);"
                         "  const _17 = _14 * _16;"
                         "  const _18 = -1.0;"
                         "  const _19 = _18 * p06;"
                         "  const _20 = _19 * _13;"
                         "  const _21 = _17 + _20;"
                         "  const _22 = _21 / p05;"
                         "  const _23 = [_09, y03, _22];"
                         "  return _23;"]))]
         (compile-state-fn driven/state-derivative
                            '[m l g a omega]
                            (up 't 'theta 'thetadot)
                            {:mode :js
                             :gensym-fn (a/monotonic-symbol-generator 2)})))
  (is (= ["a09" "a10" "a11"
          (maybe-defloatify
           (s/join "\n" ["  const y01 = a09[0];"
                         "  const y02 = a09[1];"
                         "  const y03 = a09[2];"
                         "  const p04 = a11[0];"
                         "  const p05 = a11[1];"
                         "  const p06 = a11[2];"
                         "  const p07 = a11[3];"
                         "  const p08 = a11[4];"
                         "  const _12 = 1.0;"
                         "  const _13 = 2.0;"
                         "  const _14 = Math.pow(p08, _13);"
                         "  const _15 = p07 * _14;"
                         "  const _16 = Math.sin(y02);"
                         "  const _17 = _15 * _16;"
                         "  const _18 = p08 * y01;"
                         "  const _19 = Math.cos(_18);"
                         "  const _20 = _17 * _19;"
                         "  const _21 = -1.0;"
                         "  const _22 = _21 * p06;"
                         "  const _23 = _22 * _16;"
                         "  const _24 = _20 + _23;"
                         "  const _25 = _24 / p05;"
                         "  a10[0] = _12;"
                         "  a10[1] = y03;"
                         "  a10[2] = _25;"]
                   ))]
         (compile-state-fn driven/state-derivative
                            '[m l g a omega]
                            (up 't 'theta 'thetadot)
                            {:mode :js
                             :calling-convention :primitive
                             :gensym-fn (a/monotonic-symbol-generator 2)})))

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
         (compile-state-fn
          #(e/Hamiltonian->state-derivative
            (e/Lagrangian->Hamiltonian
             (driven/L 'm 'l 'g 'a 'omega)))
          []
          (e/->H-state 't 'theta 'p_theta)
          {:mode :js
           :gensym-fn (a/monotonic-symbol-generator 2)}))))
