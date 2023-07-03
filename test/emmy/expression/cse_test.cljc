(ns emmy.expression.cse-test
  (:require [clojure.test :refer [is deftest]]
            [emmy.expression.analyze :as a]
            [emmy.expression.cse :as c]))

(deftest subexp-tests
  (is (= '[g4 ([g1 (+ x y)] [g2 (+ x z)] [g3 (* g1 g2)] [g4 (* g3 g1)])]
         (c/extract-common-subexpressions
          '(* (+ x y) (+ x z) (+ x y))
          vector
          {:gensym-fn (a/monotonic-symbol-generator 1 "g")}))
      "common (+ x y) variable is extracted.")

  (let [expr '(+ (* (sin x) (cos x))
                 (* (sin x) (cos x))
                 (* (sin x) (cos x)))
        opts {:gensym-fn (a/monotonic-symbol-generator 1 "g")}
        slimmed 'g5
        expected-subs '([g1 (sin x)]
                        [g2 (cos x)]
                        [g3 (* g1 g2)]
                        [g4 (+ g3 g3)]
                        [g5 (+ g4 g3)])]
    (is (= [slimmed expected-subs]
           (c/extract-common-subexpressions expr vector opts))
        "nested subexpressions are extracted in order, and the substitution map
        is suitable for a let binding (and has no extra variables)."))

  (let [expr '(+ (sin x) (expt (sin x) 2)
                 (cos x) (sqrt (cos x)))
        opts {:gensym-fn (a/monotonic-symbol-generator 1 "K")}
        slimmed 'K8
        expected-subs '([K1 (sin x)]
                        [K2 2]
                        [K3 (expt K1 K2)]
                        [K4 (+ K1 K3)]
                        [K5 (cos x)]
                        [K6 (+ K4 K5)]
                        [K7 (sqrt K5)]
                        [K8 (+ K6 K7)])]
    (is (= [slimmed expected-subs]
           (c/extract-common-subexpressions expr vector opts))
        "subexpressions are again extracted in order.")))
