#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.cse
  "This namespace implements subexpression extraction and \"elimination\", the
  process we use to avoid redundant computation inside of a simplified function
  body."
  (:require #?(:cljs [goog.string :refer [format]])
            [clojure.set :as set]
            [clojure.walk :as w]
            [emmy.expression :as x]
            [emmy.expression.analyze :as a]
            [emmy.util :as u]
            [taoensso.timbre :as log]))

;;  The goal of this process is to split some symbolic expression into:
;;
;;  - a map of symbol -> redundant subexpression
;;  - a new expression with each redundant subexpr replaced with its
;;    corresponding symbol.
;;
;;  The invariant we want to achieve is that the new expression, rehydrated using
;;  the symbol->subexpression map, should be equivalent to the old expression.
;;
;;  First, we write a function to determine how often each subexpression appears.
;;  Subexpressions that appear just once aren't worth extracting, but two
;;  appearances is enough.

(defn- expr-frequencies
  "Returns a map from distinct subexpressions in the supplied `expr` to the
  number of times each appears.

  If `skip?` returns true given a subexpression it won't be included as a key in
  the returned map."
  [expr skip?]
  (let [children (partial filter seq?)]
    (->> (rest
          (tree-seq seq? children expr))
         (remove skip?)
         (frequencies))))

;; The next function assumes that we have the two data structures we referenced
;; earlier. We want to be careful that we only generate and bind subexpressions
;; that are actually used in the final computation.
;;
;; `discard-unferenced-syms` ensures this by removing any entry from our
;; replacement map that doesn't appear in the expression it's passed, or any
;; subexpression referenced by a symbol in the expression, etc etc.
;;
;; The algorithm is:
;;
;; - consider the intersection of all variables in the replacement map and the
;;   supplied expression, and store these into a map of "referenced" symbols.
;;
;; - Look up the corresponding subexpression for each of these symbols, and add
;;   all potential replacements from each subexpression into the "referenced"
;;   list, continuing this process recursively until all candidates are
;;   exhausted.
;;
;; - Return subset of the original `sym->expr` by removing any key not found in
;; - the accumulated "referenced" set.

(defn- discard-unreferenced-syms
  "Trims down a proposed map of `sym->expr` replacements (suitable for a let-binding,
  say) to include only entries relevant to the supplied `expr`.

  Accepts:

  - `sym->expr`, a map of symbol -> symbolic expression
  - `expr`, an expression that potentially contains symbols referenced in the
    keyset of `sym->expr`

  And returns a subset of `sym->expr` containing only entries where the
  symbol-key is found in:

  - the original `expr`, or
  - in an expression referenced by a symbol in the original `expr`"
  [sym->expr expr]
  (let [syms        (u/keyset sym->expr)
        lookup-syms (mapcat (comp x/variables-in sym->expr))]
    (loop [referenced #{}
           sym-batch  (-> (x/variables-in expr)
                          (set/intersection syms))]
      (if (empty? sym-batch)
        (select-keys sym->expr referenced)
        (let [referenced'   (set/union referenced sym-batch)
              syms-in-exprs (-> (into #{} lookup-syms sym-batch)
                                (set/intersection syms)
                                (set/difference referenced'))]
          (recur referenced' syms-in-exprs))))))

;; This final function implements common subexpression extraction in
;; continuation-passing-style. Pass it a callback and it will invoke the
;; callback with the two arguments described above, and detailed below in its
;; docstring.
;;
;; The algorithm is:
;;
;; - For every subexpression that appears more than once in the supplied
;;   expression, generate a new, unique symbol.
;;
;; - Generate a new expression by replacing every subexpression in the supplied
;;   expression with a symbol using the new mapping of symbol -> subexpression.
;;
;; - Recursively keep going until there are no more common subexpressions to
;;   replace. At this point, discard all extra bindings (see
;;   `discard-unreferenced-syms` above) and call the continuation function with
;;   the /new/ slimmed expression, and a sorted-by-symbol list of binding pairs.
;;
;; These two return values satisfy the invariant we described above: the new
;; expression, rehydrated using the symbol->subexpression map, should give us
;; back the old expression.
;;
;; NOTE that the algorithm as implemented below uses a postwalk tree traversal!
;; This is important, as it forces us to consider smaller subexpressions first.
;; Consider some expression like:

#_(+ (* (sin x) (cos x))
     (* (sin x) (cos x))
     (* (sin x) (cos x)))

;; At first pass, we have three repeated subexpressions:
;;
;; - `(sin x)`
;; - `(cos x)`
;; - `(* (sin x) (cos x))`
;;
;; Postwalk traversal guarantees that we replace the `sin` and `cos` terms
;; before the larger term that contains them both. And in fact the returned pair
;; looks like:

#_[(+ g3 g3 g3) ([g1 (sin x)] [g2 (cos x)] [g3 (* g1 g2)])]

;; NOTE also that:
;;
;; - this is why the `:symbol-generator` below must generate symbols that sort
;;   in the order they're generated. Else, the final binding vector might put
;;   the `g3` term in the example above /before/ the smaller subexpressions it
;;   uses.
;;
;; - This algorithm justifies `discard-unreferenced-syms` above. Each pass will
;;   larger subexpressions like `'(* (sin x) (cos x))` that should never make it
;;   out, since they never appear in this form (since they contain smaller
;;   subexpressions).

(def sortable-gensym
  (a/monotonic-symbol-generator "G"))

(defn extract-common-subexpressions
  "Considers an S-expression from the point of view of optimizing its evaluation
  by isolating common subexpressions into auxiliary variables.

  Accepts:

  - A symbolic expression `expr`
  - a continuation fn `continue` of two arguments:
    - a new equivalent expression with possibly some subexpressions replaced by
      new variables (delivered by the supplied generator, see below)
    - a seq of pairs of `[aux variable, subexpression]` used to reconstitute the
      value.

  Calls the continuation at completion and returns the continuation's value.

  ### Optional Arguments

  `:symbol-generator`: side-effecting function that returns a new, unique
  variable name on each invocation. `sortable-gensym` by default.

  NOTE that the symbols should appear in sorted order! Otherwise we can't
  guarantee that the binding sequence passed to `continue` won't contain entries
  that reference previous entries.

  `:deterministic?`: if true, the function will assign aux variables by sorting
  the string representations of each term before assignment. Otherwise, the
  nondeterministic order of hash maps inside this function won't guarantee a
  consistent variable naming convention in the returned function. For tests, set
  `:deterministic? true`."
  ([expr continue] (extract-common-subexpressions expr continue {}))
  ([expr continue {:keys [symbol-generator deterministic?]
                   :or {symbol-generator sortable-gensym}}]
   (let [sort (if deterministic?
                (partial sort-by (comp str vec first))
                identity)]
     (loop [x         expr
            expr->sym {}]
       (let [expr->count (expr-frequencies x expr->sym)
             new-syms    (into {} (for [[k v] (sort expr->count)
                                        :when (> v 1)]
                                    [k (symbol-generator)]))]
         (if (empty? new-syms)
           (let [sym->expr (-> (set/map-invert expr->sym)
                               (discard-unreferenced-syms x))]
             (continue x (sort-by key sym->expr)))
           (let [expr->sym' (merge expr->sym new-syms)]
             (recur (w/postwalk-replace expr->sym' x)
                    expr->sym'))))))))

;; This final wrapper function invokes `extract-common-subexpressions` to turn a
;; symbolic expression a new, valid Clojure(script) form that uses a `let`
;; binding to bind any common subexpressions exposed during the above search.
;;
;; If there are no common subexpressions, `cse-form` will round-trip its input.

(defn cse-form
  "Given a symbolic expression `expr`, returns a new expression potentially
  wrapped in a `let` binding with one binding per extracted common
  subexpression.

  ## Optional Arguments

  `:symbol-generator`: side-effecting function that returns a new, unique symbol
  on each invocation. These generated symbols are used to create unique binding
  names for extracted subexpressions. `sortable-gensym` by default.

  NOTE that the symbols should appear in sorted order! Otherwise we can't
  guarantee that the binding sequence won't contain entries that reference
  previous entries, resulting in \"Unable to resolve symbol\" errors.

  `:deterministic?`: if true, the function will order the let-binding contents
  by sorting the string representations of each term before assignment. If false
  the function won't guarantee a consistent variable naming convention in the
  returned function. For tests, we recommend `:deterministic? true`."
  ([expr] (cse-form expr {}))
  ([expr opts]
   (letfn [(callback [new-expression bindings]
             (let [n-bindings (count bindings)]
               (if (pos? n-bindings)
                 (let [binding-vec (into [] cat bindings)]
                   (log/info
                    (format "common subexpression elimination: %d expressions" n-bindings))
                   `(let ~binding-vec
                      ~new-expression))
                 new-expression)))]
     (extract-common-subexpressions expr callback opts))))
