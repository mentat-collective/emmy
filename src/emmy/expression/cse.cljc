#_"SPDX-License-Identifier: GPL-3.0"

^#:nextjournal.clerk
{:toc true
 :visibility :hide-ns}
(ns emmy.expression.cse
  "This namespace implements subexpression extraction and \"elimination\", the
  process we use to avoid redundant computation inside of a simplified function
  body."
  (:require [emmy.expression.analyze :as a]
            [mentat.clerk-utils :refer [->clerk-only]]))

;; ## Common Sub-Expression Elimination

;; We implement the algorithm described in \[F90\]:
;;
;;   [Analytic Variations on the Common Subexpression Problem]
;;   (https://algo.inria.fr/flajolet/Publications/FlSiSt90.pdf)
;;   Philippe Flajolet, Paolo Sipala, Jean-Marc Steyaert (1990)
;;
;; From the abstract:
;;
;; > Any tree can be represented in a maximally compact form as
;; > a directed acyclic graph where common subtrees are factored and
;; > shared, being represented only once. Such a compaction can be
;; > effected in linear time. It is used to save storage in
;; > implementations of functional programming languages, as
;; > well as in symbolic manipulation and computer algebra systems.
;;
;; which is exactly what we want.
;;
;; The heart of the algorithm is a function which computes a unique
;; identifier (UID) for a subtree, such that isomorphic subtrees receive the
;; same UID. If we regard each node of the tree as the root of such a
;; subtree, the UID assignment will have the effect of partitioning
;; the nodes of the tree into equivalence classes. We can then produce
;; a DAG encoding of the original tree by using these class indices.
;;
;; The paper assumes the input trees are binary, but Emmy expressions
;; often have sums and products in which the `+` and `*` functions are
;; applied to long strings of arguments. We have observed that such long
;; chains of operands can frustrate this kind of optimization, by hiding
;; common (permuted) subsequences of operand within two similar
;; subexpressions of a commutative operation.
;;
;; We begin by rewriting our input expression to avoid this. While this
;; will likely generate a tree with more nodes, it exposes each primitive
;; computation to the algorithm which enables a maximal factoring (at the
;; cost, as we will see, of generating code in which operations are limited
;; to two operands. This may produce less compact-looking compiled code,
;; but should be perfectly transparent to the code generators of the host
;; languages.)

(defn- dissociate
  "If `x` is an expression like `(f x1 x2 x3...)`, where `(f? f)` is true,
  returns `(f ... (f x1 x2) x3 ...)`, i.e., we replace applications with
  arity > 2 with left-associative nested applications of arity 2. Otherwise
  `x` is returned."
  [f? x]
  (letfn [(go [x]
              (cond
                (not (seq? x)) x

                (or (not (f? (first x)))
                    (< (count x) 4))
                (map go x)

                :else (let [[f & xs] x]
                        (reduce (partial list f) (map go xs)))))]
    (go x)))

;; This is the heart of the F90 algorithm. We do a post-order traversal
;; of the tree, building an association between equivalence classes of
;; subtree to small integers. We can then represent the maximally factored
;; DAG as a simple sequence of expressions, where integers in argument
;; position represent back-references to the indexed computation emitted
;; previously (that is, in the nth expression, indices in the range [0..n-1]
;; may appear). Thus the computation may be efficiently performed by
;; evaluating the subexpressions in the order reported.

(defn- uid-assigner
  "Produces a function which takes an S-expression and produces the
  compacted DAG representation of the tree, as described in [Flajolet 90].
  Returns the ID of the root of the tree. The assigner generated by this
  function can be used multiple times, and earlier compact forms will be reused
  by new expressions; in this way, multiple expressions in the same scope
  may be compacted.

  Passing no argument will \"dump\" the current table as a sequence of
  simple forms with a function symbol at the head followed by arguments
  (or a constant). The arguments are either symbols drawn from the original
  expression or integers which represent the the index of the value of a
  computation performed earlier in the sequence."
  []
  (let [counter (atom -1)
        table   (atom {})]
    (letfn [(install [x]
              (or (@table x)
                  (let [i (swap! counter inc)]
                    (swap! table assoc x i)
                    i)))
            (uid [x]
                (cond
                  (seq? x) (install (into [(first x)] (map uid) (next x)))
                  (number? x) (install x)
                  :else x))]
      (fn
        ([] (->> @table (sort-by second) (map first)))
        ([expr] (uid (dissociate '#{+ *} expr)))))))

;; An example will make this process clear:

(->clerk-only
 (let [u (uid-assigner)]
   (u (dissociate '#{+ *}
                  '(+ (* (sin x) (cos x))
                      (* (sin x) (cos x))
                      (* (sin x) (cos x)))))
   (u)))

;; This produces the sequence
;;
;; `[sin x] [cos x] [* 0 1] [+ 2 2] [+ 3 2]`
;;
;; Which we may interpret as follows:
;;
;; `(sin x)` and `(cos x)` are computed and considered values 0 and 1, respectively.
;; To get value 2, we multiply values 0 and 1: `(* (sin x) (cos x))`
;; To get value 3, we add value 2 to itself, and to get value 4, we add values 3 and 2.
;; This is the "unrolled form" of the sum of the three identical elements.
;;
;; What about numbers that may appear in the original expression? We've tweaked the
;; algorithm to stash such numbers into an indexed sub-computation so that there is
;; no ambiguity

(->clerk-only
 (let [u (uid-assigner)]
   (u '(+ (expt a 2) (expt a 3)))
   (u)))

;; We get
;;
;; `(2 [expt a 0] 3 [expt a 2] [+ 1 3])`
;;
;; in which the constants 2 and 3 are stored in values 0 and 2, and referred to
;; by those indices when needed.  This is reminiscent of a technique called the
;; "literal pool," used by compilers on architectures which lack immediate-mode
;; instructions, so that values have to be loaded by address. (It does have the
;; side effect of coalescing the use of the same constant more than once in a
;; single variable, but the platform code generators probably do not need our
;; help with that.)

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

  The special form `(doto v (aset i v_i)...)` is recognized at the top level,
  and the CSE process is then confined to the $v_i$ expressions.

  ### Optional Arguments

  `:gensym-fn`: side-effecting function that returns a new, unique
  variable name prefixed by its argument on each invocation.
  `monotonic-symbol-generator` by default."
  [expr continue {:keys [gensym-fn]
                  :or {gensym-fn (a/monotonic-symbol-generator 8 "_")}}]
  (let [u (uid-assigner)]
    (letfn
     [(dag->symbols-and-values
        ;; After we're done feeding the expression(s) to the UID assigner,
        ;; we get a list of constants and function application forms with
        ;; arguments which are either symbols or integer placeholders. The
        ;; integers represent the index of a sub-computation earlier in the
        ;; list. We generate symbols and paste them over the integers here.
        [continue]
        (let [dag     (u)
              n       (count dag)
              symbols (into [] (repeatedly n gensym-fn))
              subst   (fn [x] (if (integer? x) (symbols x) x))
              values  (for [d dag]
                        (if (vector? d)
                          (list* (first d) (map subst (next d)))
                          d))]
          (continue symbols values)))
      (handle-single-expression
        ;; The simple case
        []
        (u expr)
        (dag->symbols-and-values
         (fn [symbols values] (continue (last symbols) (map vector symbols values)))))
      (handle-doto-statement
        ;; In this case we want to feed the right hand sides of all the `aset`
        ;; statements within the `doto` to the UID assigner, generate the needed
        ;; quantity of new symbols, and then paste the symbols into the RHS
        ;; position of the `aset`s. The new `doto` statement becomes the new
        ;; expression; all the supporting computation is effected by evaluating
        ;; the subexpressions.
        []
        (let [[_doto array-symbol & aset-statements] expr
              indexed-uids (doall (for [[_aset index expr] aset-statements]
                                    [index (u expr)]))]
          (dag->symbols-and-values
           (fn [symbols values]
             (continue
              `(doto ~array-symbol ~@(map (fn [[i j]] `(aset ~i ~(if (integer? j) (symbols j) j))) indexed-uids))
              (map vector symbols values))))))]
      (cond (not (sequential? expr)) (continue expr nil)
            (= (first expr) `doto) (handle-doto-statement)
            :else (handle-single-expression)))))
