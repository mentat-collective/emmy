(ns emmy.expression.cse-test
  (:require #?(:cljs [goog.string :refer [format]])
            [clojure.test :refer [is deftest]]
            [clojure.walk :as w]
            [emmy.expression.cse :as c]))

(defn ^:private make-generator
  [s]
  (let [i (atom 0)]
    (fn []
      (symbol (format "%s%d" s (swap! i inc))))))

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
          {:deterministic? true
           :symbol-generator (make-generator "g")}))
      "common (+ x y) variable is extracted.")

  (let [expr '(+ (* (sin x) (cos x))
                 (* (sin x) (cos x))
                 (* (sin x) (cos x)))
        opts {:deterministic? true
              :symbol-generator (make-generator "g")}
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
        opts {:deterministic? true
              :symbol-generator (make-generator "K")}
        slimmed '(+ K2 (expt K2 2) K1 (sqrt K1))
        expected-subs '([K1 (cos x)]
                        [K2 (sin x)])]
    (is (= expr (rehydrate slimmed (into {} expected-subs)))
        "The substitutions are correct.")

    (is (= [slimmed expected-subs]
           (c/extract-common-subexpressions expr vector opts))
        "subexpressions are again extracted in order.")))

(def letsym
  #?(:clj 'clojure.core/let :cljs 'cljs.core/let))

(deftest subexp-compile-tests
  (let [expr '(+ (* (sin x) (cos x))
                 (* (sin x) (cos x))
                 (* (sin x) (cos x))
                 (sin x)
                 (expt (sin x) 2)
                 (cos x)
                 (sqrt (cos x))
                 (tan x))]
    (is (= (list letsym
                 '[g2 (sin x)
                   g3 (cos x)
                   g4 (* g2 g3)]
                 '(+ g4 g4 g4
                     g2 (expt g2 2)
                     g3 (sqrt g3) (tan x)))
           (c/cse-form expr {:symbol-generator (make-generator "g")}))
        "Bindings appear in the correct order for subs.")

    (is (= '(+ a b (sin x) (cos y))
           (c/cse-form '(+ a b (sin x) (cos y))))
        "No substitutions means no let binding.")))
