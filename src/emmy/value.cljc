#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.value
  "The home of most of the protocol-based extensible generic operations offered by
  Emmy. The bulk of the others live in [[emmy.generic]].

  See [the `Generics`
  cljdocs](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/basics/generics)
  for a detailed discussion of how to use and extend the generic operations
  defined in [[emmy.generic]] and [[emmy.value]]."
  (:refer-clojure :exclude [zero? number? = compare])
  (:require #?@(:cljs [["complex.js" :as Complex]
                       ["fraction.js/bigfraction.js" :as Fraction]
                       [emmy.util :as u]
                       [goog.array :as garray]
                       [goog.object :as gobject]
                       [goog.math.Long]
                       [goog.math.Integer]])
            [clojure.core :as core])
  #?(:clj
     (:import
      (clojure.lang BigInt Sequential Var)
      (org.apache.commons.math3.complex Complex))))

(defprotocol Numerical
  (^boolean numerical? [_]))

(extend-protocol Numerical
  #?(:clj Object :cljs default)
  (numerical? [_] false))

(defprotocol IKind
  (kind [this]))

(defn argument-kind [& args]
  (mapv kind args))

(def object-name-map (atom {}))

(def seqtype #?(:clj Sequential :cljs ::seq))

;; Allows multimethod dispatch to seqs in CLJS.
#?(:cljs
   (do
     (derive Cons ::seq)
     (derive IndexedSeq ::seq)
     (derive PersistentVector ::seq)
     (derive LazySeq ::seq)
     (derive List ::seq)
     (derive Range ::seq)))

;; Smaller inheritance tree to enabled shared implementations between numeric
;; types that represent mathematical integers.

(derive ::native-integral ::integral)
(derive ::integral ::real)
(derive ::floating-point ::real)
(derive ::real ::number)

(defn native-integral?
  "Returns true if x is an integral number that Clojure's math operations work
  with, false otherwise."
  [x]
  (integer? x))

(defn integral?
  "Returns true if x is an integral number, false otherwise."
  [x]
  #?(:clj (integer? x)
     :cljs (or (int? x)
               (core/= "bigint" (goog/typeOf x)))))

(defn real?
  "Returns true if `x` is either an integral number or a floating point number (i.e.,
  in the numeric tower but not complex), false otherwise."
  [x]
  #?(:clj (instance? Number x)
     :cljs (or (cljs.core/number? x)
               (instance? goog.math.Integer x)
               (instance? goog.math.Long x)
               (core/= "bigint" (goog/typeOf x))
               (instance? Fraction x))))

(defn number?
  "Returns true if `x` is any number type in the numeric tower:

  - integral
  - floating point
  - complex

  false otherwise."
  [x]
  #?(:clj
     (or (instance? Number x)
         (instance? Complex x))
     :cljs (or (cljs.core/number? x)
               (core/= "bigint" (goog/typeOf x))
               (instance? Fraction x)
               (instance? goog.math.Integer x)
               (instance? goog.math.Long x)
               (instance? Complex x))))

;; `::scalar` is a thing that symbolic expressions AND actual numbers both
;; derive from.

(derive ::number ::scalar)

(defn scalar?
  "Returns true for anything that derives from `::scalar`, i.e., any numeric type in
  the numeric tower that responds true to [[number?]], plus symbolic expressions
  generated by [[emmy.abstract.number/literal-number]],

  false otherwise."
  [x]
  (isa? (kind x) ::scalar))

#?(:clj
   (do
     (derive Number ::real)
     (derive Double ::floating-point)
     (derive Float ::floating-point)
     (derive BigDecimal ::floating-point)
     (derive Integer ::native-integral)
     (derive Long ::native-integral)
     (derive BigInt ::native-integral)
     (derive BigInteger ::native-integral))

   :cljs
   (do (derive js/Number ::real)
       (derive js/BigInt ::integral)
       (derive goog.math.Integer ::integral)
       (derive goog.math.Long ::integral)))

(extend-protocol Numerical
  #?(:clj Number :cljs number)
  (numerical? [_] true)

  #?@(:clj
      [java.lang.Double
       (numerical? [_] true)

       java.lang.Float
       (numerical? [_] true)]))

(extend-protocol IKind
  #?(:clj Number :cljs number)
  (kind [x] #?(:clj (type x)
               :cljs (if (and (. js/Number isInteger x)
                              (< (Math/abs x) (.-MAX_SAFE_INTEGER js/Number)))
                       ::native-integral
                       ::floating-point)))

  #?(:clj Boolean :cljs boolean)
  (kind [x] (type x))

  #?@(:clj
      [java.lang.Double
       (kind [x] (type x))

       java.lang.Float
       (kind [x] (type x))])

  nil
  (kind [_] nil)

  Var
  (kind [v] (type v))

  #?(:clj Object :cljs default)
  (kind [o] (:type o (type o))))


;; Override equiv for numbers.
(defmulti = argument-kind)

;; These two constitute the default cases.
(defmethod = [::number ::number] [l r]
  #?(:clj  (== l r)
     :cljs (identical? l r)))

(defmethod = [seqtype seqtype] [l r]
  (and (= (count l) (count r))
       (every? true? (map = l r))))

(defmethod = :default [l r]
  (if (or (isa? (kind l) ::number)
          (isa? (kind r) ::number))
    false
    (core/= l r)))

#?(:cljs
   ;; These definitions are required for the protocol implementation below.
   (do
     (defmethod = [::native-integral js/BigInt] [l r]
       (coercive-= l r))

     (defmethod = [js/BigInt ::native-integral] [l r]
       (coercive-= l r))

     (doseq [[from to f] [[goog.math.Long goog.math.Integer u/int]
                          [::native-integral goog.math.Integer u/int]
                          [::native-integral goog.math.Long u/long]
                          [goog.math.Long js/BigInt u/bigint]
                          [goog.math.Integer js/BigInt u/bigint]]]
       (defmethod = [from to] [l r] (core/= (f l) r))
       (defmethod = [to from] [l r] (core/= l (f r))))

     (defmethod = [goog.math.Long goog.math.Long]
       [^goog.math.Long l ^goog.math.Long r]
       (.equals l r))

     (defmethod = [goog.math.Integer goog.math.Integer]
       [^goog.math.Integer l ^goog.math.Integer r]
       (.equals l r))

     (extend-protocol IEquiv
       number
       (-equiv [this other]
         (cond (core/number? other) (identical? this other)
               (numerical? other)   (= this (.valueOf other))
               :else false))

       goog.math.Integer
       (-equiv [this other]
         (if (core/= goog.math.Integer (type other))
           (.equals this other)
           (= this (.valueOf other))))

       goog.math.Long
       (-equiv [this other]
         (if (core/= goog.math.Long (type other))
           (.equals this other)
           (= this (.valueOf other)))))))

#?(:cljs
   (extend-type js/BigInt
     IHash
     (-hash [this] (hash (.toString this 16)))

     IEquiv
     (-equiv [this o]
       (let [other (.valueOf o)]
         (if (u/bigint? other)
           (coercive-= this other)
           (= this other))))

     IPrintWithWriter
     (-pr-writer [x writer _]
       (let [rep (if (< (if (< x 0) (- x) x) (.-MAX_SAFE_INTEGER js/Number))
                   (str x)
                   (str "\"" x "\""))]
         (write-all writer "#emmy/bigint " rep)))))

#?(:cljs
   ;; goog.math.{Long, Integer} won't compare properly using <, > etc unless they
   ;; can convert themselves to numbers via `valueOf.` This extension takes care of
   ;; that modification.
   (do
     (extend-type goog.math.Long
       IHash
       (-hash [this] (.hashCode this))

       Object
       (valueOf [this] (.toNumber this)))

     (extend-type goog.math.Integer
       IHash
       (-hash [this] (hash (.toString this 16)))

       Object
       (valueOf [this] (.toNumber this)))))

#?(:cljs
   (extend-protocol IComparable
     number
     (-compare [this o]
       (let [other (.valueOf o)]
         (if (real? other)
           (garray/defaultCompare this other)
           (throw (js/Error. (str "Cannot compare " this " to " o))))))

     js/BigInt
     (-compare [this o]
       (let [other (.valueOf o)]
         (if (real? other)
           (garray/defaultCompare this other)
           (throw (js/Error. (str "Cannot compare " this " to " o))))))

     goog.math.Integer
     (-compare [this o]
       (let [other (.valueOf o)]
         (cond (instance? goog.math.Integer other) (.compare this other)
               (real? other) (garray/defaultCompare this other)
               :else (throw (js/Error. (str "Cannot compare " this " to " o))))))

     goog.math.Long
     (-compare [this o]
       (let [other (.valueOf o)]
         (cond (instance? goog.math.Long other) (.compare this other)
               (real? other) (garray/defaultCompare this other)
               :else (throw (js/Error. (str "Cannot compare " this " to " o))))))))

#?(:cljs
   ;; ClojureScript-specific implementations of Value.
   (do
     (extend-protocol Numerical
       js/BigInt
       (numerical? [_] true)

       goog.math.Integer
       (numerical? [_] true)

       goog.math.Long
       (numerical? [_] true))

     (extend-protocol IKind
       js/BigInt
       (kind [_] js/BigInt)

       goog.math.Integer
       (kind [_] goog.math.Integer)

       goog.math.Long
       (kind [_] goog.math.Long))))

#?(:cljs
  ;; We find it convenient to be able to decorate "vanilla" JavaScript
  ;; functions with metadata. In Clojurescript, using `with-meta` on a
  ;; native function will replace it with an AFn (abstract function)
  ;; instance, basically an object holding the function and its metadata
  ;; in two ordinary properties. Such an object does not have the Function
  ;; prototype, and is therefore not directly callable inJS.
  ;;
  ;; To fix this, we allow native functions to store metadata
  ;; in a symbol-indexed property. The Clojurescript implementation
  ;; is arguably more "hygienic," and gets around the difficulty of
  ;; cloning the function as the `IWithMeta` contract would require,
  ;; but this implementation should be safe enough for our purposes.
  ;;
  ;; By using a symbol instead of a string as the key for the metadata,
  ;; the metadata annotation will not be visible among the object's other
  ;; normal properties.
   (let [metadata-symbol (. js/Symbol for "Symbol.__emmy_meta__")]
     (extend-type function
       IMeta
       (-meta [f] (or (gobject/get f metadata-symbol) nil)))

    ;; We would like to support IWithMeta, but that interface's contract
    ;; requires the production of a _new_ object. Instead we provide a function
    ;; to mutate a native object's metadata in place.
     (defn set-js-meta!
       "Mutates the native JS object `o` to have the given metadata. The
       previous metadata, if any, is discarded. `o` is returned."
       [o m]
       (unchecked-set o metadata-symbol m)
       o))
   )

#?(:cljs
   (defn make-es6-callable
     "Make s callable. This is done by re-hosting all of the object properties of `s`
      in a new native JS function which delegates to the Clojure application. The
      result of the application of this new function is supplied to the continuation `k`."
     [s k]
     (let [f (fn [& xs] (k (apply s xs)))]
       (. js/Object setPrototypeOf f (. js/Object getPrototypeOf s))
       (doseq [property-name (. js/Object getOwnPropertyNames s)]
         (unchecked-set f property-name (gobject/get s property-name)))
       (doseq [property-symbol (. js/Object getOwnPropertySymbols s)]
         (unchecked-set f property-symbol (gobject/get s property-symbol)))
       f)))

(defn kind-predicate
  "Returns a predicate that returns true if its argument matches the supplied
  kind-keyword `k`, false otherwise."
  [x]
  (let [k (kind x)]
    (fn [x2] (isa? (kind x2) k))))

#?(:clj
   (defn compare
     "Comparator. Returns a negative number, zero, or a positive number
  when x is logically 'less than', 'equal to', or 'greater than'
  y. Same as Java x.compareTo(y) except it also works for nil, and
  compares numbers and collections in a type-independent manner. x
  must implement Comparable"
     [x y]
     (if (core/number? x)
       (if (core/number? y)
         (core/compare x y)
         (- (core/compare y x)))
       (core/compare x y)))
   :cljs
   (defn ^number compare
     "Comparator. Clone of [[cljs.core/compare]] that works with the expanded
      Emmy numeric tower.

  Returns a negative number, zero, or a positive number when x is logically
  'less than', 'equal to', or 'greater than' y. Uses IComparable if available
  and google.array.defaultCompare for objects of the same type and special-cases
  nil to be less than any other object."
     [x y]
     (cond
       (identical? x y) 0
       (nil? x)         -1
       (nil? y)         1
       (core/number? x) (let [yv (.valueOf y)]
                          (if (real? yv)
                            (garray/defaultCompare x yv)
                            (throw (js/Error. (str "Cannot compare " x " to " y)))))

       (satisfies? IComparable x)
       (-compare x y)

       :else
       (if (and (or (string? x) (array? x) (true? x) (false? x))
                (identical? (type x) (type y)))
         (garray/defaultCompare x y)
         (throw (js/Error. (str "Cannot compare " x " to " y)))))))

(defn add-object-symbols!
  [o->syms]
  (swap! object-name-map into o->syms))

(defn within
  "Returns a function that tests whether two values are within ε of each other."
  [^double ε]
  (fn [^double x ^double y]
    (< (Math/abs (- x y)) ε)))

(def twopi (* 2 Math/PI))

(defn principal-value [cuthigh]
  (let [cutlow (- cuthigh twopi)]
    (fn [x]
      (if (and (<= cutlow x) (< x cuthigh))
        x
        (let [y (- x (* twopi (Math/floor (/ x twopi))))]
          (if (< y cuthigh)
            y
            (- y twopi)))))))
