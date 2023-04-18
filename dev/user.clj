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
  ;; Evaluate this form when working on performance-intensive code.
  (alter-var-root #'*warn-on-reflection* (constantly true)))

(def index
  "dev/emmy/notebook.clj")

(def notebooks
  ["src/emmy/**/**.cljc"
   "src/emmy/collection.cljc"
   ;; "src/emmy/complex.cljc"
   ;; "src/emmy/env.cljc"
   ;; "src/emmy/euclid.cljc"
   ;; "src/emmy/function.cljc"
   ;; "src/emmy/generic.cljc"
   ;; "src/emmy/numbers.cljc"
   ;; "src/emmy/numsymb.cljc"
   ;; "src/emmy/ratio.cljc"
   ;; "src/emmy/sci.cljc"
   ;; "src/emmy/simplify.cljc"
   ;; "src/emmy/util.cljc"
   ;; "src/emmy/value.cljc"

   ;; NOTE: including any namespace with a deftype causes the static build to
   ;; fail. We'll need to fix this with the Clerk team...
   ;; "src/emmy/expression.cljc"
   ;; "src/emmy/differential.cljc"
   ;; "src/emmy/matrix.cljc"
   ;; "src/emmy/modint.cljc"
   ;; "src/emmy/operator.cljc"
   ;; "src/emmy/polynomial.cljc"
   ;; "src/emmy/quaternion.cljc"
   ;; "src/emmy/rational_function.cljc"
   ;; "src/emmy/series.cljc"
   ;; "src/emmy/structure.cljc"
   ])

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
