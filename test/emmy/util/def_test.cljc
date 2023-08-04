(ns emmy.util.def-test
  (:require [clojure.test :refer [deftest is]]
            [emmy.util :as u]
            [emmy.util.def :as d]))

(d/import-def u/keyset keyset)
(d/import-vars [emmy.util map-vals])

(deftest target
   (is (= #{:a :b} (keyset {:a 1 :b 2})))
   (is (= {:a 1} (map-vals inc {:a 0}))))