#_"SPDX-License-Identifier: GPL-3.0"

^#:nextjournal.clerk
  {:toc true
   :visibility :hide-ns}
(ns emmy.abstract.function
  "Implementation of a [[literal-function]] constructor. Literal functions can be
  applied to structures and numeric inputs, and differentiated.

  The namespace also contains an implementation of a small language for
  declaring the input and output types of [[literal-function]] instances."
  (:refer-clojure :exclude [name])
  (:require #?(:clj [clojure.pprint :as pprint])
            [emmy.abstract.number :as an]
            [emmy.autodiff :as ad]
            [emmy.dual :as dual]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.matrix :as m]
            [emmy.numsymb :as sym]
            [emmy.polynomial]
            [emmy.structure :as s]
            [emmy.tape :as tape]
            [emmy.util :as u]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang IFn)))
  #?(:cljs
     (:require-macros [emmy.abstract.function])))

;; ## Abstract Function
;;
;; This namespace declares an abstract function type, along with the support
;; structure to process the scmutils domain/range language.

(declare literal-apply f:=)

;; This derivation allows `::function` to take advantage of all generic
;; operations installed via [[emmy.function]].

(derive ::function ::v/function)

(defn ^:private sicm-set->exemplar
  "Convert a SICM-style set (e.g., Real or (UP Real Real)) to
  an exemplar (an instance of the relevant type)."
  [s]
  (cond
    (= s 'Real) 0

    (sequential? s)
    (let [[constructor & args] s]
      (case constructor
        X     (mapv sicm-set->exemplar args)
        UP    (apply s/up (map sicm-set->exemplar args))
        DOWN  (apply s/down (map sicm-set->exemplar args))
        UP*   (apply s/up (repeat (second args) (sicm-set->exemplar (first args))))
        DOWN* (apply s/down (repeat (second args) (sicm-set->exemplar (first args))))
        X*    (into [] (repeat (second args) (sicm-set->exemplar (first args))))))))

(defn ^:no-doc sicm-signature->domain-range
  "Convert a SICM-style literal function signature (e.g.,
  '(-> Real (X Real Real)) ) to our 'exemplar' format."
  [[arrow domain range]]
  (when-not (and (= '-> arrow) domain range)
    (u/illegal (str "A SICM signature is of the form '(-> domain range), got: " arrow domain range)))
  [(let [d (sicm-set->exemplar domain)]
     (if (vector? d) d [d]))
   (sicm-set->exemplar range)])

(deftype Function [f-name arity domain range]
  v/IKind
  (kind [_] ::function)

  f/IArity
  (arity [_] arity)

  Object
  (toString [_] (str f-name))
  #?(:clj (equals [a b] (f:= a b)))

  #?@(:clj
      [IFn
       (invoke [this x] (literal-apply this [x]))
       (invoke [this x y] (literal-apply this [x y]))
       (invoke [this x y z] (literal-apply this [x y z]))
       (invoke [this w x y z] (literal-apply this [w x y z]))
       (applyTo [this xs] (literal-apply this xs))]

      :cljs
      [IEquiv
       (-equiv [a b] (f:= a b))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer (.toString x)))

       IFn
       (-invoke [this a]
                (literal-apply this [a]))
       (-invoke [this a b]
                (literal-apply this [a b]))
       (-invoke [this a b c]
                (literal-apply this [a b c]))
       (-invoke [this a b c d]
                (literal-apply this [a b c d]))
       (-invoke [this a b c d e]
                (literal-apply this [a b c d e]))
       (-invoke [this a b c d e f]
                (literal-apply this [a b c d e f]))
       (-invoke [this a b c d e f g]
                (literal-apply this [a b c d e f g]))
       (-invoke [this a b c d e f g h]
                (literal-apply this [a b c d e f g h]))
       (-invoke [this a b c d e f g h i]
                (literal-apply this [a b c d e f g h i]))
       (-invoke [this a b c d e f g h i j]
                (literal-apply this [a b c d e f g h i j]))
       (-invoke [this a b c d e f g h i j k]
                (literal-apply this [a b c d e f g h i j k]))
       (-invoke [this a b c d e f g h i j k l]
                (literal-apply this [a b c d e f g h i j k l]))
       (-invoke [this a b c d e f g h i j k l m]
                (literal-apply this [a b c d e f g h i j k l m]))
       (-invoke [this a b c d e f g h i j k l m n]
                (literal-apply this [a b c d e f g h i j k l m n]))
       (-invoke [this a b c d e f g h i j k l m n o]
                (literal-apply this [a b c d e f g h i j k l m n o]))
       (-invoke [this a b c d e f g h i j k l m n o p]
                (literal-apply this [a b c d e f g h i j k l m n o p]))
       (-invoke [this a b c d e f g h i j k l m n o p q]
                (literal-apply this [a b c d e f g h i j k l m n o p q]))
       (-invoke [this a b c d e f g h i j k l m n o p q r]
                (literal-apply this [a b c d e f g h i j k l m n o p q r]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s]
                (literal-apply this [a b c d e f g h i j k l m n o p q r s]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t]
                (literal-apply this [a b c d e f g h i j k l m n o p q r s t]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t rest]
                (literal-apply this (concat [a b c d e f g h i j k l m n o p q r s t]  rest)))]))

#?(:clj
   (defmethod print-method Function [^Function f ^java.io.Writer w]
     (.write w (.toString f))))

#?(:clj
   ;; NOTE that this override only works in Clojure. In cljs, `simple-dispatch`
   ;; isn't extensible.
   (defmethod pprint/simple-dispatch Function [f]
     (pprint/simple-dispatch
      (.-f-name ^Function f))))

(derive Function ::function)

(defn literal-function?
  "Returns true if the supplied object is an instance of [[Function]], false
  otherwise."
  [f]
  (instance? Function f))

(defn- name
  "Returns the `-name` field of the supplied [[Function]] object. Errors if any
  other type is supplied."
  [f]
  {:pre [(literal-function? f)]}
  (.-f-name ^Function f))

(defn- domain-types
  "Returns the `-domain` field of the supplied [[Function]] object. Errors if any
  other type is supplied."
  [f]
  {:pre [(literal-function? f)]}
  (.-domain ^Function f))

(defn- range-type
  "Returns the `-range` field of the supplied [[Function]] object. Errors if any
  other type is supplied."
  [f]
  {:pre
   [(literal-function? f)]}
  (.-range ^Function f))

(defn- f:=
  "Returns true if the function `a` equals `b`, false otherwise."
  [a b]
  (and (literal-function? b)
       (= (name a) (name b))
       (= (domain-types a) (domain-types b))
       (= (range-type a) (range-type b))))

(defn literal-function
  ([f] (->Function f [:exactly 1] [0] 0))
  ([f signature]
   (let [[domain range] (sicm-signature->domain-range signature)]
     (literal-function f domain range)))
  ([f domain range]
   (cond (number? range)
         (let [arity (if (vector? domain)
                       (count domain)
                       1)]
           (->Function f [:exactly arity]
                       (if (vector? domain) domain [domain])
                       range))

         (s/structure? range)
         (let [n           (count range)
               orientation (s/orientation range)
               template    (s/literal f n orientation)]

           (s/mapr #(literal-function %1 domain %2)
                   template
                   range))
         :else
         (u/illegal (str "WTF range" range)))))

(defn ^:no-doc binding-pairs [litfns]
  (letfn [(extract-sym [entry]
            (if (symbol? entry) entry (first entry)))
          (entry->fn [entry]
            (cond (symbol? entry) `(literal-function (quote ~entry))

                  (and (sequential? entry)
                       (= (count entry) 3))
                  (let [[sym domain range] entry]
                    `(literal-function (quote ~sym) ~domain ~range))

                  :else (u/illegal (str "unknown literal function type" entry))))]
    (mapv (fn [entry]
            [(extract-sym entry)
             (entry->fn entry)])
          litfns)))

(u/sci-macro with-literal-functions
  [litfns & body]
  (let [pairs    (binding-pairs litfns)
        bindings (into [] cat pairs)]
    `(let ~bindings ~@body)))

;; ## Differentiation of literal functions

(defn- literal-partial [f path]
  (let [fexp (if (= (f/arity f) [:exactly 1]) ;; univariate
               (if (= (first path) 0)
                 (if (= (count path) 1)
                   ;; Special-case the single argument case, or a unary function
                   ;; that's provided with a structure of a single entry.
                   (sym/derivative (name f))
                   `((~'partial ~@(next path)) ~(name f)))
                 (u/illegal "wrong indices"))
               ;; If the function takes multiple arguments we DO need to index
               ;; into that first layer. (else the first layer is added.)
               `((~'partial ~@path) ~(name f)))]
    (->Function
     fexp (f/arity f) (domain-types f) (range-type f))))

(defn- forward-mode-fold
  "Takes

    - a literal function `f`
    - a structure `primal-s` of the primal components of the args to `f` (with
      respect to `tag`)
    - the `tag` of the innermost active derivative call

  And returns a folding function (designed for use
  with [[emmy.structure/fold-chain]]) that

  generates a new [[emmy.dual/Dual]] by applying the chain rule and
  summing the partial derivatives for each perturbed argument in the input
  structure."
  [f primal-s tag]
  (fn
    ([] 0)
    ([tangent] (dual/bundle-element (apply f primal-s) tangent tag))
    ([tangent [x path _]]
     (let [dx (dual/tangent x tag)]
       (if (g/numeric-zero? dx)
         tangent
         (let [partial (literal-partial f path)]
           (g/+ tangent (g/* (literal-apply partial primal-s)
                             dx))))))))

(defn- reverse-mode-fold
  "Takes

    - a literal function `f`
    - a structure `primal-s` of the primal components of the args to `f` (with
      respect to `tag`)
    - the `tag` of the innermost active derivative call

  And returns a folding function (designed for use
  with [[emmy.structure/fold-chain]]) that assembles all partial derivatives of
  `f` into a new [[emmy.tape/TapeCell]]."
  [f primal-s tag]
  (fn
    ([] [])
    ([partials]
     (tape/make tag (apply f primal-s) partials))
    ([partials [entry path _]]
     (if (and (tape/tape? entry) (= tag (tape/tape-tag entry)))
       (let [partial (literal-partial f path)]
         (conj partials [entry (literal-apply partial primal-s)]))
       partials))))

(defn- literal-derivative
  "Takes

    - a literal function `f`
    - a structure `s` of arguments
    - the `tag` of the innermost active derivative call
    - an instance of a perturbation `dx` associated with `tag`

  and generates the proper return value for `((D f) xs)`.

  In forward-mode AD this is a new [[emmy.dual/Dual]] generated by
  applying the chain rule and summing the partial derivatives for each perturbed
  argument in the input structure.

  In reverse-mode, this is a new [[emmy.tape/TapeCell]] containing a sequence of
  pairs of each input paired with the partial derivative of `f` with respect to
  that input."
  [f s tag dx]
  (let [fold-fn  (cond (tape/tape? dx) reverse-mode-fold
                       (dual/dual? dx)    forward-mode-fold
                       :else           (u/illegal "No tape or differential inputs."))
        primal-s (s/mapr (fn [x] (ad/primal-of x tag)) s)]
    (s/fold-chain (fold-fn f primal-s tag) s)))

(defn- check-argument-type
  "Check that the argument provided at index i has the same type as
  the exemplar expected."
  [f provided expected indexes]
  (cond (number? expected)
        (when-not (v/scalar? provided)
          (u/illegal (str "expected numerical quantity in argument " indexes
                          " of function call " f
                          " but got " provided)))

        (s/structure? expected)
        (do (when-not (and (or (s/structure? provided) (sequential? provided))
                           (= (s/orientation provided) (s/orientation expected))
                           (= (count provided) (count expected)))
              (u/illegal (str "expected structure matching " expected
                              " but got " provided)))
            (doseq [[provided expected sub-index] (map list provided expected (range))]
              (check-argument-type f provided expected (conj indexes sub-index))))

        (keyword? expected) ;; a keyword has to match the argument's kind
        (when-not (= (v/kind provided) expected)
          (u/illegal (str "expected argument of type " expected " but got " (v/kind provided)
                          " in call to function " f)))

        :else (u/illegal (str "unexpected argument example. got " provided " want " expected))))

(defn- literal-apply [f xs]
  (check-argument-type f xs (domain-types f) [0])
  (let [s (m/seq-> xs)]
    (if-let [[tag dx] (s/fold-chain
                       (fn
                         ([] [])
                         ([acc]     (apply ad/tag+perturbation acc))
                         ([acc [d]] (conj acc d)))
                       s)]
      (literal-derivative f s tag dx)
      (an/literal-number `(~(name f) ~@(map g/freeze xs))))))

;; ## Specific Generics
;;
;; We can install one more method - [[emmy.generic/simplify]] returns its
;; argument with the internally captured name simplified.

(defmethod g/simplify [::function] [f]
  (->Function (g/simplify (name f))
              (f/arity f)
              (domain-types f)
              (range-type f)))

(defmethod g/zero-like [::function] [^Function a] (fn [& _] (g/zero-like (.-range a))))
(defmethod g/one-like [::function] [^Function a] (fn [& _] (g/one-like (.-range a))))
(defmethod g/identity-like [::function] [^Function a]
  (let [meta {:arity (.-arity a) :from :identity-like}]
    (with-meta identity meta)))
(defmethod g/exact? [::function] [a] (f/compose g/exact? a))
(defmethod g/freeze [::function] [^Function a] (g/freeze (.-f-name a)))
