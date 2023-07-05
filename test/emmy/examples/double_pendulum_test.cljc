#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.double-pendulum-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is use-fixtures]]
            [emmy.env :as e :refer [/ down up]]
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
  #?(:cljs (s/replace s #"\b(\d+)\.0\b" (fn [[_ m]] m))
     :clj s))

(deftest infix-forms
  (is (=
       ["[y01, [y02, y03], [y04, y05]]"
        (maybe-defloatify
         (s/join "\n" [
          "  const _06 = 1.0;"
          "  const _07 = [y04, y05];"
          "  const _08 = -1.0;"
          "  const _09 = _08 * l1;"
          "  const _10 = _09 * m2;"
          "  const _11 = 2.0;"
          "  const _12 = Math.pow(y04, _11);"
          "  const _13 = _10 * _12;"
          "  const _14 = _08 * y03;"
          "  const _15 = y02 + _14;"
          "  const _16 = Math.cos(_15);"
          "  const _17 = _13 * _16;"
          "  const _18 = Math.sin(_15);"
          "  const _19 = _17 * _18;"
          "  const _20 = _08 * l2;"
          "  const _21 = _20 * m2;"
          "  const _22 = Math.pow(y05, _11);"
          "  const _23 = _21 * _22;"
          "  const _24 = _23 * _18;"
          "  const _25 = _19 + _24;"
          "  const _26 = g * m2;"
          "  const _27 = _26 * _16;"
          "  const _28 = Math.sin(y03);"
          "  const _29 = _27 * _28;"
          "  const _30 = _25 + _29;"
          "  const _31 = _08 * g;"
          "  const _32 = _31 * m1;"
          "  const _33 = Math.sin(y02);"
          "  const _34 = _32 * _33;"
          "  const _35 = _30 + _34;"
          "  const _36 = _31 * m2;"
          "  const _37 = _36 * _33;"
          "  const _38 = _35 + _37;"
          "  const _39 = l1 * m2;"
          "  const _40 = Math.pow(_18, _11);"
          "  const _41 = _39 * _40;"
          "  const _42 = l1 * m1;"
          "  const _43 = _41 + _42;"
          "  const _44 = _38 / _43;"
          "  const _45 = l2 * m2;"
          "  const _46 = _45 * _22;"
          "  const _47 = _46 * _16;"
          "  const _48 = _47 * _18;"
          "  const _49 = _42 * _12;"
          "  const _50 = _49 * _18;"
          "  const _51 = _48 + _50;"
          "  const _52 = _39 * _12;"
          "  const _53 = _52 * _18;"
          "  const _54 = _51 + _53;"
          "  const _55 = g * m1;"
          "  const _56 = _55 * _16;"
          "  const _57 = _56 * _33;"
          "  const _58 = _54 + _57;"
          "  const _59 = _27 * _33;"
          "  const _60 = _58 + _59;"
          "  const _61 = _32 * _28;"
          "  const _62 = _60 + _61;"
          "  const _63 = _36 * _28;"
          "  const _64 = _62 + _63;"
          "  const _65 = _45 * _40;"
          "  const _66 = l2 * m1;"
          "  const _67 = _65 + _66;"
          "  const _68 = _64 / _67;"
          "  const _69 = [_44, _68];"
          "  const _70 = [_06, _07, _69];"
          "  return _70;"]))]
       (c/compile-state-fn double/state-derivative
                           '[m1 m2 l1 l2 g]
                           (up 't (up 'theta 'phi) (up 'thetadot 'phidot))
                           {:mode :js
                            :calling-convention :structure
                            :generic-params? false
                            :gensym-fn (a/monotonic-symbol-generator 2)})))


  (is (= ["[y01, [y02, y03], [y04, y05]]"
          "[p06, p07, p08, p09, p10]"
          (maybe-defloatify
           (s/join "\n" [
            "  const _11 = 1.0;"
            "  const _12 = [y04, y05];"
            "  const _13 = -1.0;"
            "  const _14 = _13 * p07;"
            "  const _15 = _14 * p08;"
            "  const _16 = 2.0;"
            "  const _17 = Math.pow(y04, _16);"
            "  const _18 = _15 * _17;"
            "  const _19 = _13 * y03;"
            "  const _20 = y02 + _19;"
            "  const _21 = Math.cos(_20);"
            "  const _22 = _18 * _21;"
            "  const _23 = Math.sin(_20);"
            "  const _24 = _22 * _23;"
            "  const _25 = _14 * p09;"
            "  const _26 = Math.pow(y05, _16);"
            "  const _27 = _25 * _26;"
            "  const _28 = _27 * _23;"
            "  const _29 = _24 + _28;"
            "  const _30 = p07 * p10;"
            "  const _31 = _30 * _21;"
            "  const _32 = Math.sin(y03);"
            "  const _33 = _31 * _32;"
            "  const _34 = _29 + _33;"
            "  const _35 = _13 * p06;"
            "  const _36 = _35 * p10;"
            "  const _37 = Math.sin(y02);"
            "  const _38 = _36 * _37;"
            "  const _39 = _34 + _38;"
            "  const _40 = _14 * p10;"
            "  const _41 = _40 * _37;"
            "  const _42 = _39 + _41;"
            "  const _43 = p07 * p08;"
            "  const _44 = Math.pow(_23, _16);"
            "  const _45 = _43 * _44;"
            "  const _46 = p06 * p08;"
            "  const _47 = _45 + _46;"
            "  const _48 = _42 / _47;"
            "  const _49 = p07 * p09;"
            "  const _50 = _49 * _26;"
            "  const _51 = _50 * _21;"
            "  const _52 = _51 * _23;"
            "  const _53 = _46 * _17;"
            "  const _54 = _53 * _23;"
            "  const _55 = _52 + _54;"
            "  const _56 = _43 * _17;"
            "  const _57 = _56 * _23;"
            "  const _58 = _55 + _57;"
            "  const _59 = p06 * p10;"
            "  const _60 = _59 * _21;"
            "  const _61 = _60 * _37;"
            "  const _62 = _58 + _61;"
            "  const _63 = _31 * _37;"
            "  const _64 = _62 + _63;"
            "  const _65 = _36 * _32;"
            "  const _66 = _64 + _65;"
            "  const _67 = _40 * _32;"
            "  const _68 = _66 + _67;"
            "  const _69 = _49 * _44;"
            "  const _70 = p06 * p09;"
            "  const _71 = _69 + _70;"
            "  const _72 = _68 / _71;"
            "  const _73 = [_48, _72];"
            "  const _74 = [_11, _12, _73];"
            "  return _74;"]))]
         (c/compile-state-fn
          double/state-derivative
          '[1 1 1 1 'g]
          (up 't (up 'theta 'phi) (up 'thetadot 'phidot))
          {:mode :js
           :gensym-fn (a/monotonic-symbol-generator 2)})))

  (let [eq (e/simplify
            ((e/Hamiltonian->state-derivative
              (e/Lagrangian->Hamiltonian
               (double/L 'm_1 'm_2 'l_1 'l_2 'g)))
             (e/->H-state 't (up 'theta 'psi) (down 'p_theta 'p_psi))))]
    (is (= "up(1, up((- l₁ p_ψ cos(ψ - θ) + l₂ p_θ) / (l₁² l₂ m₂ sin²(ψ - θ) + l₁² l₂ m₁), (- l₂ m₂ p_θ cos(ψ - θ) + l₁ m₁ p_ψ + l₁ m₂ p_ψ) / (l₁ l₂² m₂² sin²(ψ - θ) + l₁ l₂² m₁ m₂)), down((- g l₁³ l₂² m₁ m₂² (cos(ψ - θ))⁴ sin(θ) - g l₁³ l₂² m₂³ (cos(ψ - θ))⁴ sin(θ) + 2 g l₁³ l₂² m₁² m₂ cos²(ψ - θ) sin(θ) + 4 g l₁³ l₂² m₁ m₂² cos²(ψ - θ) sin(θ) + 2 g l₁³ l₂² m₂³ cos²(ψ - θ) sin(θ) - g l₁³ l₂² m₁³ sin(θ) - 3 g l₁³ l₂² m₁² m₂ sin(θ) - 3 g l₁³ l₂² m₁ m₂² sin(θ) - g l₁³ l₂² m₂³ sin(θ) + l₁ l₂ m₂ p_ψ p_θ cos²(ψ - θ) sin(ψ - θ) - l₁² m₁ p_ψ² cos(ψ - θ) sin(ψ - θ) - l₁² m₂ p_ψ² cos(ψ - θ) sin(ψ - θ) - l₂² m₂ p_θ² cos(ψ - θ) sin(ψ - θ) + l₁ l₂ m₁ p_ψ p_θ sin(ψ - θ) + l₁ l₂ m₂ p_ψ p_θ sin(ψ - θ)) / (l₁² l₂² m₂² (cos(ψ - θ))⁴ + 2 l₁² l₂² m₁ m₂ sin²(ψ - θ) - 2 l₁² l₂² m₂² cos²(ψ - θ) + l₁² l₂² m₁² + l₁² l₂² m₂²), (- g l₁² l₂³ m₂³ (cos(ψ - θ))⁴ sin(ψ) - 2 g l₁² l₂³ m₁ m₂² sin²(ψ - θ) sin(ψ) + 2 g l₁² l₂³ m₂³ cos²(ψ - θ) sin(ψ) - g l₁² l₂³ m₁² m₂ sin(ψ) - g l₁² l₂³ m₂³ sin(ψ) - l₁ l₂ m₂ p_ψ p_θ cos²(ψ - θ) sin(ψ - θ) + l₁² m₁ p_ψ² cos(ψ - θ) sin(ψ - θ) + l₁² m₂ p_ψ² cos(ψ - θ) sin(ψ - θ) + l₂² m₂ p_θ² cos(ψ - θ) sin(ψ - θ) - l₁ l₂ m₁ p_ψ p_θ sin(ψ - θ) - l₁ l₂ m₂ p_ψ p_θ sin(ψ - θ)) / (l₁² l₂² m₂² (cos(ψ - θ))⁴ + 2 l₁² l₂² m₁ m₂ sin²(ψ - θ) - 2 l₁² l₂² m₂² cos²(ψ - θ) + l₁² l₂² m₁² + l₁² l₂² m₂²)))"
           (e/->infix eq)))

    (is (= ["[y001, [y002, y003], [y004, y005]]"
            "_"
            (maybe-defloatify
             (s/join "\n" ["  const _006 = 1.0;"
                           "  const _007 = -1.0;"
                           "  const _008 = _007 * l_1;"
                           "  const _009 = _008 * y005;"
                           "  const _010 = _007 * y003;"
                           "  const _011 = y002 + _010;"
                           "  const _012 = Math.cos(_011);"
                           "  const _013 = _009 * _012;"
                           "  const _014 = l_2 * y004;"
                           "  const _015 = _013 + _014;"
                           "  const _016 = 2.0;"
                           "  const _017 = Math.pow(l_1, _016);"
                           "  const _018 = _017 * l_2;"
                           "  const _019 = _018 * m_2;"
                           "  const _020 = Math.sin(_011);"
                           "  const _021 = Math.pow(_020, _016);"
                           "  const _022 = _019 * _021;"
                           "  const _023 = _018 * m_1;"
                           "  const _024 = _022 + _023;"
                           "  const _025 = _015 / _024;"
                           "  const _026 = _007 * l_2;"
                           "  const _027 = _026 * m_2;"
                           "  const _028 = _027 * y004;"
                           "  const _029 = _028 * _012;"
                           "  const _030 = l_1 * m_1;"
                           "  const _031 = _030 * y005;"
                           "  const _032 = _029 + _031;"
                           "  const _033 = l_1 * m_2;"
                           "  const _034 = _033 * y005;"
                           "  const _035 = _032 + _034;"
                           "  const _036 = Math.pow(l_2, _016);"
                           "  const _037 = l_1 * _036;"
                           "  const _038 = Math.pow(m_2, _016);"
                           "  const _039 = _037 * _038;"
                           "  const _040 = _039 * _021;"
                           "  const _041 = _037 * m_1;"
                           "  const _042 = _041 * m_2;"
                           "  const _043 = _040 + _042;"
                           "  const _044 = _035 / _043;"
                           "  const _045 = [_025, _044];"
                           "  const _046 = _007 * g;"
                           "  const _047 = 3.0;"
                           "  const _048 = Math.pow(l_1, _047);"
                           "  const _049 = _046 * _048;"
                           "  const _050 = _049 * _036;"
                           "  const _051 = _050 * m_1;"
                           "  const _052 = _051 * _038;"
                           "  const _053 = 4.0;"
                           "  const _054 = Math.pow(_012, _053);"
                           "  const _055 = _052 * _054;"
                           "  const _056 = Math.sin(y002);"
                           "  const _057 = _055 * _056;"
                           "  const _058 = Math.pow(m_2, _047);"
                           "  const _059 = _050 * _058;"
                           "  const _060 = _059 * _054;"
                           "  const _061 = _060 * _056;"
                           "  const _062 = _057 + _061;"
                           "  const _063 = _016 * g;"
                           "  const _064 = _063 * _048;"
                           "  const _065 = _064 * _036;"
                           "  const _066 = Math.pow(m_1, _016);"
                           "  const _067 = _065 * _066;"
                           "  const _068 = _067 * m_2;"
                           "  const _069 = Math.pow(_012, _016);"
                           "  const _070 = _068 * _069;"
                           "  const _071 = _070 * _056;"
                           "  const _072 = _062 + _071;"
                           "  const _073 = _053 * g;"
                           "  const _074 = _073 * _048;"
                           "  const _075 = _074 * _036;"
                           "  const _076 = _075 * m_1;"
                           "  const _077 = _076 * _038;"
                           "  const _078 = _077 * _069;"
                           "  const _079 = _078 * _056;"
                           "  const _080 = _072 + _079;"
                           "  const _081 = _065 * _058;"
                           "  const _082 = _081 * _069;"
                           "  const _083 = _082 * _056;"
                           "  const _084 = _080 + _083;"
                           "  const _085 = Math.pow(m_1, _047);"
                           "  const _086 = _050 * _085;"
                           "  const _087 = _086 * _056;"
                           "  const _088 = _084 + _087;"
                           "  const _089 = -3.0;"
                           "  const _090 = _089 * g;"
                           "  const _091 = _090 * _048;"
                           "  const _092 = _091 * _036;"
                           "  const _093 = _092 * _066;"
                           "  const _094 = _093 * m_2;"
                           "  const _095 = _094 * _056;"
                           "  const _096 = _088 + _095;"
                           "  const _097 = _092 * m_1;"
                           "  const _098 = _097 * _038;"
                           "  const _099 = _098 * _056;"
                           "  const _100 = _096 + _099;"
                           "  const _101 = _059 * _056;"
                           "  const _102 = _100 + _101;"
                           "  const _103 = _008 * l_2;"
                           "  const _104 = _103 * m_2;"
                           "  const _105 = _104 * y004;"
                           "  const _106 = _105 * y005;"
                           "  const _107 = _106 * _069;"
                           "  const _108 = _107 * _020;"
                           "  const _109 = _102 + _108;"
                           "  const _110 = _017 * m_1;"
                           "  const _111 = Math.pow(y005, _016);"
                           "  const _112 = _110 * _111;"
                           "  const _113 = _112 * _012;"
                           "  const _114 = _113 * _020;"
                           "  const _115 = _109 + _114;"
                           "  const _116 = _017 * m_2;"
                           "  const _117 = _116 * _111;"
                           "  const _118 = _117 * _012;"
                           "  const _119 = _118 * _020;"
                           "  const _120 = _115 + _119;"
                           "  const _121 = _036 * m_2;"
                           "  const _122 = Math.pow(y004, _016);"
                           "  const _123 = _121 * _122;"
                           "  const _124 = _123 * _012;"
                           "  const _125 = _124 * _020;"
                           "  const _126 = _120 + _125;"
                           "  const _127 = _103 * m_1;"
                           "  const _128 = _127 * y004;"
                           "  const _129 = _128 * y005;"
                           "  const _130 = _129 * _020;"
                           "  const _131 = _126 + _130;"
                           "  const _132 = _106 * _020;"
                           "  const _133 = _131 + _132;"
                           "  const _134 = _017 * _036;"
                           "  const _135 = _134 * _038;"
                           "  const _136 = _135 * _054;"
                           "  const _137 = _016 * _017;"
                           "  const _138 = _137 * _036;"
                           "  const _139 = _138 * m_1;"
                           "  const _140 = _139 * m_2;"
                           "  const _141 = _140 * _021;"
                           "  const _142 = _136 + _141;"
                           "  const _143 = -2.0;"
                           "  const _144 = _143 * _017;"
                           "  const _145 = _144 * _036;"
                           "  const _146 = _145 * _038;"
                           "  const _147 = _146 * _069;"
                           "  const _148 = _142 + _147;"
                           "  const _149 = _134 * _066;"
                           "  const _150 = _148 + _149;"
                           "  const _151 = _150 + _135;"
                           "  const _152 = _133 / _151;"
                           "  const _153 = _046 * _017;"
                           "  const _154 = Math.pow(l_2, _047);"
                           "  const _155 = _153 * _154;"
                           "  const _156 = _155 * _058;"
                           "  const _157 = _156 * _054;"
                           "  const _158 = Math.sin(y003);"
                           "  const _159 = _157 * _158;"
                           "  const _160 = _143 * g;"
                           "  const _161 = _160 * _017;"
                           "  const _162 = _161 * _154;"
                           "  const _163 = _162 * m_1;"
                           "  const _164 = _163 * _038;"
                           "  const _165 = _164 * _021;"
                           "  const _166 = _165 * _158;"
                           "  const _167 = _159 + _166;"
                           "  const _168 = _063 * _017;"
                           "  const _169 = _168 * _154;"
                           "  const _170 = _169 * _058;"
                           "  const _171 = _170 * _069;"
                           "  const _172 = _171 * _158;"
                           "  const _173 = _167 + _172;"
                           "  const _174 = _155 * _066;"
                           "  const _175 = _174 * m_2;"
                           "  const _176 = _175 * _158;"
                           "  const _177 = _173 + _176;"
                           "  const _178 = _156 * _158;"
                           "  const _179 = _177 + _178;"
                           "  const _180 = l_1 * l_2;"
                           "  const _181 = _180 * m_2;"
                           "  const _182 = _181 * y004;"
                           "  const _183 = _182 * y005;"
                           "  const _184 = _183 * _069;"
                           "  const _185 = _184 * _020;"
                           "  const _186 = _179 + _185;"
                           "  const _187 = _007 * _017;"
                           "  const _188 = _187 * m_1;"
                           "  const _189 = _188 * _111;"
                           "  const _190 = _189 * _012;"
                           "  const _191 = _190 * _020;"
                           "  const _192 = _186 + _191;"
                           "  const _193 = _187 * m_2;"
                           "  const _194 = _193 * _111;"
                           "  const _195 = _194 * _012;"
                           "  const _196 = _195 * _020;"
                           "  const _197 = _192 + _196;"
                           "  const _198 = _007 * _036;"
                           "  const _199 = _198 * m_2;"
                           "  const _200 = _199 * _122;"
                           "  const _201 = _200 * _012;"
                           "  const _202 = _201 * _020;"
                           "  const _203 = _197 + _202;"
                           "  const _204 = _180 * m_1;"
                           "  const _205 = _204 * y004;"
                           "  const _206 = _205 * y005;"
                           "  const _207 = _206 * _020;"
                           "  const _208 = _203 + _207;"
                           "  const _209 = _183 * _020;"
                           "  const _210 = _208 + _209;"
                           "  const _211 = _210 / _151;"
                           "  const _212 = [_152, _211];"
                           "  const _213 = [_006, _045, _212];"
                           "  return _213;"]
                     ))]
           (c/compile-state-fn
            #(e/Hamiltonian->state-derivative
              (e/Lagrangian->Hamiltonian
               (double/L 'm_1 'm_2 'l_1 'l_2 'g)))
            []
            (e/->H-state 't (up 'theta 'psi) (down 'p_theta 'p_psi))
            {:mode :js
             :gensym-fn (a/monotonic-symbol-generator 3)})))))
