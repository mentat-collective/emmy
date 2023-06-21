(ns emmy.expression.cse-test
  (:require [clojure.test :refer [is deftest]]
            [clojure.walk :as w]
            [emmy.expression.analyze :as a]
            [emmy.expression.cse :as c]))

(defn- rehydrate
  "Takes a slimmed-down expression and a potentially-multi-level substitution map
  and rebuilds the original expression."
  [slimmed sym->expr]
  (let [substitute (partial w/postwalk-replace sym->expr)]
    (reduce #(if (= %1 %2) (reduced %1) %2)
            (iterate substitute slimmed))))

(deftest subexp-tests
  (is (= '[(* g1 (+ x z) g1) ([g1 (+ x y)])]
         (c/extract-common-subexpressions
          '(* (+ x y) (+ x z) (+ x y))
          vector
          {:gensym-fn (a/monotonic-symbol-generator 1 "g")}))
      "common (+ x y) variable is extracted.")

  (let [expr '(+ (* (sin x) (cos x))
                 (* (sin x) (cos x))
                 (* (sin x) (cos x)))
        opts {:gensym-fn (a/monotonic-symbol-generator 1 "g")}
        slimmed '(+ g4 g4 g4)
        expected-subs '([g2 (cos x)]
                        [g3 (sin x)]
                        [g4 (* g3 g2)])

        sym->subexpr  (into {} expected-subs)]
    (is (= [slimmed expected-subs]
           (c/extract-common-subexpressions expr vector opts))
        "nested subexpressions are extracted in order, and the substitution map
        is suitable for a let binding (and has no extra variables).")

    (is (= expr (rehydrate slimmed sym->subexpr))
        "Rehydrating the slimmed expression should result in the original
        expression."))

  (let [expr '(+ (sin x) (expt (sin x) 2)
                 (cos x) (sqrt (cos x)))
        opts {:gensym-fn (a/monotonic-symbol-generator 1 "K")}
        slimmed '(+ K2 (expt K2 2) K1 (sqrt K1))
        expected-subs '([K1 (cos x)]
                        [K2 (sin x)])]
    (is (= expr (rehydrate slimmed (into {} expected-subs)))
        "The substitutions are correct.")

    (is (= [slimmed expected-subs]
           (c/extract-common-subexpressions expr vector opts))
        "subexpressions are again extracted in order.")))
