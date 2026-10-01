#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.render-trifecta-test
  "Golden, property and mutation tests for the extension points of
  [[emmy.expression.render/TeX-renderer]]: new infix operators, decorators and
  n-ary `and` / `or`.

  One malli schema, [[Proposition]], describes the expressions under test. It
  is the generator for every property below and the oracle that checks the
  hand-written golden cases."
  (:require [clojure.string :as s]
            [clojure.test :refer [is deftest]]
            [clojure.walk :as walk]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.env]
            [emmy.expression.render :as r]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; ## Schema

(def Proposition
  "A propositional formula. `and` and `or` take two or more arguments, and
  `(color c x)` is a decorator around the formula `x`."
  [:schema
   {:registry
    {::prop
     [:or
      [:enum 'P 'Q 'R 'S]
      [:cat [:= 'not] [:schema [:ref ::prop]]]
      ;; Two explicit arguments, then any number more. `[:repeat {:min 2}]`
      ;; generates an empty argument list once the recursion limit is hit.
      [:cat [:enum 'and 'or]
       [:schema [:ref ::prop]] [:schema [:ref ::prop]] [:* [:schema [:ref ::prop]]]]
      [:cat [:enum 'implies 'iff] [:schema [:ref ::prop]] [:schema [:ref ::prop]]]
      [:cat [:= 'color] [:enum 'gold 'red] [:schema [:ref ::prop]]]]}}
   [:ref ::prop]])

(def proposition
  "Generator of [[Proposition]]s."
  (mg/generator Proposition {:size 30}))

;; ## Renderers

(def ^:private logic-options
  {:infix? '#{implies iff}
   :precedence-map '{implies 0 iff -1}
   :special-handlers {'implies #(s/join " \\Rightarrow " %)
                      'iff #(s/join " \\Leftrightarrow " %)}})

(defn- logic-renderer [& {:as options}]
  (apply r/TeX-renderer (mapcat identity (merge-with merge logic-options options))))

(def ->logic
  "The subject: a TeX renderer for [[Proposition]]s that colours its
  decorated subterms."
  (logic-renderer
   :decorators {'color (fn [[c x]] (str "\\textcolor{" c "}{" x "}"))}))

(def ^:private ->uncolored
  "Renders like [[->logic]], with a decorator that leaves no trace."
  (logic-renderer :decorators {'color (fn [[_ x]] x)}))

(defn- strip-color
  "Replaces each `(color c x)` in `expr` with `x`."
  [expr]
  (walk/postwalk
   (fn [x] (if (and (seq? x) (= 'color (first x))) (last x) x))
   expr))

(defn- occurrences [text sub]
  (loop [n 0 from 0]
    (if-let [i (s/index-of text sub from)]
      (recur (inc n) (+ i (count sub)))
      n)))

(defn- balanced?
  "True if every delimiter opened in `tex` is closed."
  [tex]
  (and (= (occurrences tex "\\left(") (occurrences tex "\\right)"))
       (= (occurrences tex "{") (occurrences tex "}"))))

;; ## Trifecta

(def ^:private cases
  '{:nary-and        (and P Q R)
    :nary-or         (or P Q R S)
    :implies-binds-loosest (implies (and P Q) P)
    :implies-under-and (and (implies P Q) P)
    :contrapositive  (iff (implies P Q) (implies (not Q) (not P)))
    :color-or-under-and (and (color gold (or P Q)) R)
    :color-and-under-or (or (color gold (and P Q)) R)
    :color-atom      (color gold P)
    :color-nested    (and (color red (color gold (or P Q))) R)})

(deftrifecta logic-TeX
  #'->logic
  {:golden-path "test/golden/render/logic-tex.edn"
   :cases       cases

   :gen         proposition
   :pred        balanced?

   :mutations
   [["and/or render only their first two arguments"
     (logic-renderer
      :special-handlers {'and (fn [[x y]] (str x " \\land " y))
                         'or  (fn [[x y]] (str x " \\lor " y))}
      :decorators {'color (fn [[c x]] (str "\\textcolor{" c "}{" x "}"))})]

    ["color is an ordinary function, so it hides its parent operator"
     (logic-renderer
      :special-handlers {'color (fn [[c x]] (str "\\textcolor{" c "}{" x "}"))})]

    ["new operators are not infix"
     (r/TeX-renderer
      :decorators {'color (fn [[c x]] (str "\\textcolor{" c "}{" x "}"))})]]})

;; ## Properties over two renderers

(deftest golden-cases-are-propositions
  (doseq [[label expr] cases]
    (is (m/validate Proposition expr) (str label " conforms to Proposition"))))

(deftest decorators-are-transparent
  (checking "a decorated term is parenthesized exactly as the bare term" 200
    [expr proposition]
    (is (= (->logic (strip-color expr))
           (->uncolored expr)))))

(deftest extension-keeps-the-default-renderer
  (let [->default (r/TeX-renderer)]
    (checking "a renderer built with no options is ->TeX*" 200
      [expr proposition]
      (let [expr (strip-color expr)]
        (is (= (r/->TeX* expr) (->default expr)))))))
