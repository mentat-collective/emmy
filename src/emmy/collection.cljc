#_"SPDX-License-Identifier: GPL-3.0"

^#:nextjournal.clerk
  {:toc true
   :visibility :hide-ns}
(ns emmy.collection
  "This namespace contains implementations of various Emmy protocols for
  native Clojure collections."
  (:require [clojure.set :as cs]
            [emmy.dual :as d]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.util :as u]
            [emmy.value :as v])
  #?(:clj
     (:import
      (clojure.lang PersistentVector
                    IPersistentVector
                    ISeq
                    PersistentHashSet
                    PersistentTreeSet
                    PersistentArrayMap
                    PersistentHashMap
                    PersistentTreeMap))))

;; ## Vector Implementations
;;
;; Vectors are implicitly treated as [[emmy.structure/Structure]] instances
;; with an `up` orientation, and implement [[g/freeze]] identically. They can
;; act as `zero?`, but they can't act as `one?` or `identity?`; those are
;; reserved for instances that have no effect on multiplication.

(defmethod g/simplify [PersistentVector] [v]
  (mapv g/simplify v))
(defmethod g/zero-like [PersistentVector] [v]
  (mapv g/zero-like v))
(defmethod g/exact? [PersistentVector] [v] (every? g/exact? v))
(defmethod g/freeze [PersistentVector] [v] `(~'up ~@(map g/freeze v)))

#?(:clj
   (defmethod g/simplify [clojure.lang.APersistentVector$SubVector] [v]
     (mapv g/simplify v)))

(extend-type #?(:clj IPersistentVector :cljs PersistentVector)
  v/IKind
  (kind [v] (type v))

  ;; Another difference from [[emmy.structure/Structure]] is that a
  ;; structure of functions acts as a function itself that applies its entries
  ;; to its arguments. Vectors already implement IFn (they take an index and
  ;; look it up), so they can't respond the same way as a structure via
  ;; arity. (the `2` arity takes an additional default value.)
  f/IArity
  (arity [_] [:between 1 2])

  ;; Vectors are functors, so they can be perturbed if any of their elements are
  ;; perturbed. [[d/replace-tag]] and [[d/extract-tangent]] pass the buck down
  ;; the vector's elements.
  d/IPerturbed
  (perturbed? [v] (boolean (some d/perturbed? v)))
  (replace-tag [v old new] (mapv #(d/replace-tag % old new) v))
  (extract-tangent [v tag mode] (mapv #(d/extract-tangent % tag mode) v)))

;; ## Sequences
;;
;; Sequences can't act as functions or respond to any of
;; the [[g/zero?]]-and-friends predicates. They pass along the operations that
;; they can implement to their elements via [[map]].

(defmethod g/simplify [v/seqtype] [a]
  (map g/simplify a))

(doseq [klass #?(:clj [ISeq]
                 :cljs [Cons IndexedSeq LazySeq List Range IntegerRange])]
  (defmethod g/zero? [klass] [_] false)
  (defmethod g/one? [klass] [_] false)
  (defmethod g/identity? [klass] [_] false)
  (defmethod g/zero-like [klass] [xs] (map g/zero-like xs))
  (defmethod g/exact? [klass] [xs] (every? g/exact? xs))
  (defmethod g/freeze [klass] [xs] (map g/freeze xs))

  (extend-type #?(:clj ISeq :cljs klass)
    v/IKind
    (kind [xs] (type xs))

    d/IPerturbed
    (perturbed? [_] false)
    (replace-tag [xs old new] (map #(d/replace-tag % old new) xs))
    (extract-tangent [xs tag mode] (map #(d/extract-tangent % tag mode) xs))))

;; ## Maps
;;
;; Maps acts as functors that can be perturbed and zeroed out (and pass along
;; calls to [[g/partial-derivative]] to their elements!), but not much else.
;;
;; NOTE: There is probably a case for making something
;; like [[emmy.structure/Structure]] backed by a map, for a sort of sparse
;; structure, or a dataframe-like structure with named fields instead of
;; positional fields. Nothing like this exists yet!

(derive PersistentHashMap ::map)
(derive PersistentArrayMap ::map)
(derive PersistentTreeMap ::map)

(defmethod g/negate [::map] [m]
  (u/map-vals g/negate m))

(defmethod g/add [::map ::map] [a b]
  (merge-with g/add a b))

(defmethod g/sub [::map ::map] [a b]
  (merge-with g/add a (u/map-vals g/negate b)))

(defmethod g/mul [::map ::v/scalar] [m x]
  (u/map-vals #(g/mul % x) m))

(defmethod g/mul [::v/scalar ::map] [x m]
  (u/map-vals #(g/mul x %) m))

(defmethod g/div [::map ::v/scalar] [m x]
  (u/map-vals #(g/div % x) m))

(defn- combine [f m1 m2 l-default]
  (letfn [(merge-entry [m e]
            (let [k (key e)
                  v (val e)]
              (assoc m k (f (get m k l-default) v))))]
    (reduce merge-entry m1 (seq m2))))

(defmethod g/make-rectangular [::map ::map] [m1 m2]
  (combine g/make-rectangular m1 m2 0))

(defmethod g/make-polar [::map ::map] [m1 m2]
  (combine g/make-polar m1 m2 0))

(defmethod g/real-part [::map] [m]
  (u/map-vals g/real-part m))

(defmethod g/imag-part [::map] [m]
  (u/map-vals g/imag-part m))

(defmethod g/simplify [::map] [m]
  (u/map-vals g/simplify m))

(let [sentinel #?(:cljs (NeverEquiv.)
                  :clj (Object.))]
  (defmethod v/= [::map ::map] [x y]
    (boolean
     (when (== (count x) (count y))
       (reduce-kv
        (fn [_ k v]
          (if (v/= (get y k sentinel) v)
            true
            (reduced false)))
        true
        x)))))

(defmethod g/partial-derivative [::map v/seqtype] [m selectors]
  (u/map-vals #(g/partial-derivative % selectors)
              m))

(doseq [klass [PersistentHashMap PersistentArrayMap PersistentTreeMap]]
  (defmethod g/zero? [klass] [m] (every? g/zero? (vals m)))
  (defmethod g/one? [klass] [_] false)
  (defmethod g/identity? [klass] [_] false)
  (defmethod g/zero-like [klass] [m] (u/map-vals g/zero-like m))
  (defmethod g/exact? [klass] [m] (every? g/exact? (vals m)))
  (defmethod g/freeze [klass] [m] (u/map-vals g/freeze m))

  #?(:clj
     (extend klass
       v/IKind
       {:kind (fn [m] (if (sorted? m)
                       (type m)
                       (:type m (type m))))}

       f/IArity
       {:arity (fn [_] [:between 1 2])}

       d/IPerturbed
       {:perturbed? (fn [m] (boolean (some d/perturbed? (vals m))))
        :replace-tag (fn [m old new] (u/map-vals #(d/replace-tag % old new) m))
        :extract-tangent
        (fn [m tag mode]
          (if-let [t (:type m)]
            ;; Do NOT attempt to recurse into the values if this map is being used as a
            ;; simple representation for some other type, like a manifold point.
            (u/unsupported (str "`extract-tangent` not supported for type " t "."))
            (u/map-vals #(d/extract-tangent % tag mode) m)))})

     :cljs
     (extend-type klass
       v/IKind
       (kind [m] (if (sorted? m)
                   (type m)
                   (:type m (type m))))

       f/IArity
       (arity [_] [:between 1 2])

       d/IPerturbed
       (perturbed? [m] (boolean (some d/perturbed? (vals m))))
       (replace-tag [m old new] (u/map-vals #(d/replace-tag % old new) m))
       (extract-tangent [m tag mode]
         (if-let [t (:type m)]
           ;; Do NOT attempt to recurse into the values if this map is being used as a
           ;; simple representation for some other type, like a manifold point.
           (u/unsupported (str "`extract-tangent` not supported for type " t "."))
           (u/map-vals #(d/extract-tangent % tag mode) m))))))

;; ## Sets
;;
;; Emmy treats Clojure's set data structure as a monoid, with set union as
;; the addition operation and the empty set as the zero element.

(derive PersistentHashSet ::set)
(derive PersistentTreeSet ::set)

(defmethod g/add [::set ::set] [a b]
  (cs/union a b))

(doseq [klass [PersistentHashSet PersistentTreeSet]]
  (defmethod g/zero? [klass] [s] (empty? s))
  (defmethod g/one? [klass] [_] false)
  (defmethod g/identity? [klass] [_] false)
  (defmethod g/zero-like [klass] [_] #{})
  (defmethod g/exact? [klass] [s] (every? g/exact? s))

  #?(:clj
     (extend klass
       v/IKind
       {:kind type}

       f/IArity
       {:arity (fn [_] [:between 1 2])})

     :cljs
     (extend-type klass
       v/IKind
       (kind [s] (type s))

       f/IArity
       (arity [_] [:between 1 2]))))
