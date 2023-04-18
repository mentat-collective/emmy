(ns user
  (:require [mentat.clerk-utils.build :as b]
            [nextjournal.clerk.config :as cc]))

(try (requiring-resolve 'cljs.analyzer.api/ns-resolve)
     (catch Exception _ nil))
(require '[emmy.env])

;; This is required to prevent Clerk from realizing long
;; computationally-intensive sequences on render / static build.
(alter-var-root #'cc/*bounded-count-limit*
                (constantly 10))

(comment
  (alter-var-root #'*warn-on-reflection* (constantly true)))

(def index
  "TODO: Create a meaningful index here."
  "dev/emmy/notebook.clj")

(def notebooks
  ["src/emmy/**/**.cljc"
   "src/emmy/**/**.clj"])

(def defaults
  {:index index
   :browse? true
   :watch-paths ["src" "dev"]
   #_#_
   :cljs-namespaces '[emmy.sci-extensions]})

(def static-defaults
  (assoc defaults
         :browse? false
         :paths notebooks
         :cname "emmy.mentat.org"
         :git/url "https://github.com/mentat-collective/emmy"))

(defn serve!
  ([] (serve! {}))
  ([opts]
   (b/serve!
    (merge defaults opts))))

(def halt! b/halt!)

(defn build! [opts]
  (b/build!
   (merge static-defaults opts)))
