(ns user
  (:require [mentat.clerk-utils.build :as b]))

(comment
  (alter-var-root #'*warn-on-reflection* (constantly true)))

(def index
  "Hmm, let's create this... TODO"
  "dev/index.md")

(def notebooks
  ["src/emmy/calculus/derivative.cljc"
   "src/emmy/differential.cljc"])

(def defaults
  {#_#_:index index
   :browse? true
   :watch-paths ["src" "dev"]
   :cljs-namespaces
   '[emmy.sci-extensions]})

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
