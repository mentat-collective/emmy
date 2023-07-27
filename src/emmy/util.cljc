#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.util
  "Shared utilities between clojure and clojurescript."
  (:refer-clojure :exclude [bigint biginteger double long int uuid])
  (:require #?(:clj [clojure.core :as core])
            #?(:clj [clojure.math.numeric-tower :as nt])
            #?(:cljs goog.math.Integer)
            #?(:cljs goog.math.Long)
            [clojure.walk :as w])
  #?(:clj
     (:import (clojure.lang BigInt)
              (java.util UUID)
              (java.util.concurrent TimeoutException)))
  #?(:cljs
     (:require-macros emmy.util)))

(defn counted
  "Takes a function and returns a pair of:

  - an atom that keeps track of fn invocation counts,
  - the instrumented fn"
  ([f] (counted f 0))
  ([f initial-count]
   (let [count (atom initial-count)]
     [count (fn [x]
              (swap! count inc)
              (f x))])))

(def compute-sqrt #?(:clj nt/sqrt :cljs Math/sqrt))
(def compute-expt #?(:clj nt/expt :cljs Math/pow))
(def compute-abs #?(:clj nt/abs :cljs Math/abs))
(def biginttype #?(:clj BigInt :cljs js/BigInt))
(def inttype #?(:clj Integer :cljs goog.math.Integer))
(def longtype #?(:clj Long :cljs goog.math.Long))

(defn keyset [m]
  (into #{} (keys m)))

(defn map-vals
  "Returns a map of identical type and key set to `m`, with each value `v`
  transformed by the supplied function`f` into `(f v)`."
  [f m]
  (reduce-kv (fn [acc k v]
               (assoc acc k (f v)))
             (empty m)
             m))

(defn re-matches?
  "Returns true if s matches the regex pattern re, false otherwise."
  [re s]
  #?(:clj  (.matches (re-matcher re s))
     :cljs (.test re s)))

(defn bigint [x]
  #?(:clj (core/bigint x)
     :cljs (js/BigInt x)))

(defn bigint?
  "Returns true if the supplied `x` is a `BigInt`, false otherwise."
  [x]
  #?(:clj  (instance? BigInt x)
     :cljs (= "bigint" (goog/typeOf x))))

(defn parse-bigint [x]
  `(bigint ~x))

(defn biginteger [x]
  #?(:clj (core/biginteger x)
     :cljs (js/BigInt x)))

(defn int [x]
  #?(:clj (core/int x)
     :cljs (goog.math.Integer/fromNumber x)))

(defn long [x]
  #?(:clj (core/long x)
     :cljs (goog.math.Long/fromNumber x)))

(defn double [x]
  #?(:clj (core/double x)
     :cljs (if (number? x) x (js/Number x))))

(defn unsupported [s]
  (throw
   #?(:clj (UnsupportedOperationException. ^String s)
      :cljs (js/Error s))))

(defn exception [s]
  (throw
   #?(:clj (Exception. ^String s)
      :cljs (js/Error s))))

(defn uuid
  "Returns a string containing a randomly generated unique identifier."
  []
  (str
   #?(:clj (UUID/randomUUID)
      :cljs (random-uuid))))

(defn illegal [s]
  (throw
   #?(:clj (IllegalArgumentException. ^String s)
      :cljs (js/Error s))))

(defn interrupted [s]
  (throw
   #?(:clj (InterruptedException. ^String s)
      :cljs (js/Error s))))

(defn illegal-state [s]
  (throw
   #?(:clj (IllegalStateException. ^String s)
      :cljs (js/Error s))))

(defn arithmetic-ex [s]
  (throw
   #?(:clj (ArithmeticException. s)
      :cljs (js/Error s))))

(defn timeout-ex [s]
  (throw
   #?(:clj (TimeoutException. s)
      :cljs (js/Error s))))

(defn failure-to-converge [s]
  (throw
   #?(:clj (Exception. ^String s)
      :cljs (js/Error s))))

(defn throwable? [t]
  (instance? #?(:clj Throwable :cljs js/Error) t))

(defn without-symbol-namespaces
  "Walks x, removing namespaces from any symbols that are found.
  Convenient in unit tests, where the distinction between symbols
  in `clojure.core` vs. `cljs.core` is unimportant"
  [x]
  (w/postwalk (fn [s] (if (qualified-symbol? s) (symbol (name s)) s)) x))

(defmacro sci-macro [name & body]
  (if (:ns &env)
    (let [[doc body] (if (string? (first body))
                       [(first body) (rest body)]
                       [nil body])
          [options body] (if (map? (first body))
                           [(first body) (rest body)]
                           [nil body])
          arities (if (vector? (first body)) (list body) body)
          arities (map (fn [[argv & body]] (list* (into '[&form &env] argv) body)) arities)]
      `(defn ~(vary-meta name assoc :sci/macro true)
         ~@(when doc [doc])
         ~@(when options [options])
         ~@arities))
    `(~'clojure.core/defmacro ~name ~@body)))

(defmacro copy-ns
  ([ns-sym sci-ns] `(copy-ns ~ns-sym ~sci-ns nil))
  ([ns-sym sci-ns opts]
   (list 'sci.core/copy-ns
         ns-sym
         sci-ns
         (merge {:copy-meta [:doc :arglists :macro :imported-from]} opts))))