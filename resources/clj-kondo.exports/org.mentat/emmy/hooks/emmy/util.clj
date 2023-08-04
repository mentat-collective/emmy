(ns hooks.emmy.util
  (:require [clj-kondo.hooks-api :as api]))

(defn copy-ns
  "Converts a node representing an invocation of the [[emmy.util/copy-ns]] macro
  into a vector node that quotes the `ns-sym` and `opts` entries."
  [{:keys [node]}]
  (let [[_ ns-sym sci-ns opts] (:children node)]
    {:node
     (api/vector-node
      [(api/list-node [(api/token-node 'quote) ns-sym])
       sci-ns
       (api/list-node [(api/token-node 'quote) opts])])}))
