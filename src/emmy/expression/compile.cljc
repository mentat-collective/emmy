#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.compile
  "This namespace contains tools for compiling functions implemented with the
  numeric operations defined in [[emmy.generic]] down to fast, native
  functions."
  (:require #?(:cljs [goog.string :refer [format]])
            [clojure.set :as set]
            [clojure.string :as cs]
            [clojure.walk :as w]
            [emmy.expression :as x]
            [emmy.expression.analyze :as a]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.structure :as struct]
            [emmy.util :as u]
            [emmy.util.stopwatch :as us]
            [emmy.value :as v]
            [sci.core :as sci]
            [taoensso.timbre :as log]))

;; This first bit is a small set of benchmarks exploring the speed differences
;; between the case where we pass in JS arrays vs using vectors all the way
;; through.

(defn vec->vec
  "vector in, vector out."
  [[t theta thetadot]]
  [1.0 thetadot (* -9.8 (Math/sin theta))])

(defn ->vec
  "Array in, returns a vector."
  [^doubles arr]
  (let [t        (aget arr 0)
        theta    (aget arr 1)
        thetadot (aget arr 2)]
    [1.0 thetadot (* -9.8 (Math/sin theta))]))

(defn ->array
  "Array in, internally create an array and return it."
  [^doubles arr]
  (let [t        (aget arr 0)
        theta    (aget arr 1)
        thetadot (aget arr 2)]
    (doto ^doubles (make-array Double/TYPE  3)
      (aset 0 1.0)
      (aset 1 thetadot)
      (aset 2 (* -9.8 (Math/sin theta))))))

(defn ->input-array
  "Array in, output array in, set values on the output array."
  [^doubles arr ^doubles out]
  (let [t        (aget arr 0)
        theta    (aget arr 1)
        thetadot (aget arr 2)]
    (doto out
      (aset 0 1.0)
      (aset 1 thetadot)
      (aset 2 (* -9.8 (Math/sin theta))))))

(defn ->input-array-param
  "Same thing, but with a parameter vector in supplying the 9.8."
  [params ^doubles arr ^doubles out]
  (let [t        (aget arr 0)
        theta    (aget arr 1)
        thetadot (aget arr 2)]
    (doto out
      (aset 0 1.0)
      (aset 1 thetadot)
      (aset 2 (* #?(:clj  (.nth ^clojure.lang.PersistentVector params 0)
                    :cljs (-nth params 0))

                 (Math/sin theta))))))

(comment
  ;; SO what I've learned here is that I need to both change the inputs to take
  ;; arrays, and the outputs to produce vectors if that occurs.
  (let [v [0 1 2]
        xs (double-array [0 1 2])
        n  100000000]
    (time (dotimes [_ n]
            (vec->vec v)))

    (time (dotimes [_ n]
            (->vec xs)))

    (time (dotimes [_ n]
            (->array xs)))

    (let [out ^doubles (make-array Double/TYPE 3)]
      (time (dotimes [_ n]
              (->input-array xs out))))

    (let [out ^doubles (make-array Double/TYPE 3)
          params [-9.8]]
      (time (dotimes [_ n]
              (->input-array-param params xs out)))))

  ;; On my machine:
  "Elapsed time: 1850.618375 msecs"
  "Elapsed time: 468.673792 msecs"
  "Elapsed time: 477.605167 msecs"
  "Elapsed time: 475.19675 msecs"
  "Elapsed time: 602.779916 msecs"
  )

;; I forget why I added this benchmark... this should be the same, I was seeing
;; if there was any difference if you then used the output array. It's a touch
;; faster to supply the output array, and better for memory usage as well, of
;; course.
(defn vec-checker [arr]
  (let [result (->vec arr)]
    (= (result 1)
       (result 2))))

(defn array-checker [arr]
  (let [result ^doubles (->array arr)]
    (= (aget result 1)
       (aget result 2))))

(defn input-array-checker [^doubles arr ^doubles result]
  (->input-array arr result)
  (= (aget result 1)
     (aget result 2)))

(comment
  (let [xs (double-array [0 1 2])
        n 100000000]
    (time (dotimes [_ n]
            (vec-checker xs)))

    (time (dotimes [_ n]
            (array-checker xs)))

    (let [out ^doubles (make-array Double/TYPE 3)]
      (time (dotimes [_ n]
              (input-array-checker xs out)))))

  ;; on my machine...
  "Elapsed time: 1943.508708 msecs"
  "Elapsed time: 540.407792 msecs"
  "Elapsed time: 508.390167 msecs"
  )

;; Now we're back to regularly scheduled programming.

;; # Function Compilation
;;
;; Functions built out of generic operations can be compiled down into fast,
;; efficient functions that operate on doubles. The process is:
;;
;; 1. Pass symbols like `'x` in for all arguments. This will cause the function
;;    to return a "numerical expression", a syntax tree representing the
;;    function's body:
#_
(let [f (fn [x] (g/sqrt
                (g/+ (g/square (g/sin x))
                     (g/square (g/cos x)))))]
  (= '(sqrt (+ (expt (sin x) 2) (expt (cos x) 2)))
     (x/expression-of (f 'x))))

;; 2. `g/simplify` the new function body. Sometimes this results in large
;;    simplifications:

#_
(let [f (fn [x] (g/sqrt
                (g/+ (g/square (g/sin x))
                     (g/square (g/cos x)))))]
  (v/= 1 (g/simplify (f 'x))))

;; 3. Apply "common subexpression elimination". Any subexpression inside the
;;    new, simplified body that appears more than once gets extracted out into a
;;    let-bound variable and calculated just once, then subbed in to the
;;    computation using the generated variable.
;;
;; 4. Wrap the new, efficient body up in `(fn [x] ,,,)` with the proper number
;;    of arguments and pass it to `eval`.
;;
;; Amazing!
;;
;; ## Implementation
;;
;; Function compilation is potentially expensive, so allocate a cache of
;; function `f` to compiled function for use below:

(def ^:private fn-cache (atom {}))

;; The next two forms allow us to walk an expression tree, and either:
;;
;; - namespace-resolve whitelisted symbols, or
;; - apply operations at compile time if all arguments are available (and
;;   non-symbolic).
;;
;; If we were only dealing with Clojure here, we could get both of these pieces
;; from a var. But forms like `Math/pow` make this tricky, so we go with the map
;; described below.

(def ^{:private true
       :doc "Dict of `<symbol> -> {:sym <symbolic-fn>, :f <evaluated-fn>}`. The
       keys constitute the set of operations allowed to appear within the body
       of the compiled function.

       If you're compiling a function for use with the numerical routines, the
       library assumes that your function operates only on doubles (even though
       you wrote it with generic routines)."}
  compiled-fn-whitelist
  {'up {:sym `vector :f vector}
   'down {:sym `struct/down
          :f struct/down}
   '+ {:sym `+ :f +}
   '- {:sym `- :f -}
   '* {:sym `* :f *}
   '/ {:sym `/ :f /}
   'expt {:sym 'Math/pow :f #(Math/pow %1 %2)}
   'sqrt {:sym 'Math/sqrt :f #(Math/sqrt %)}
   'abs {:sym 'Math/abs :f #(Math/abs ^double %)}
   'log {:sym 'Math/log :f #(Math/log %)}
   'exp {:sym 'Math/exp :f #(Math/exp %)}
   'cos {:sym 'Math/cos :f #(Math/cos %)}
   'sin {:sym 'Math/sin :f #(Math/sin %)}
   'tan {:sym 'Math/tan :f #(Math/tan %)}
   'acos {:sym 'Math/acos :f #(Math/acos %)}
   'asin {:sym 'Math/asin :f #(Math/asin %)}
   'atan {:sym 'Math/atan :f #(Math/atan %)}
   'cosh {:sym 'Math/cosh :f #(Math/cosh %)}
   'sinh {:sym 'Math/sinh :f #(Math/sinh %)}
   'tanh {:sym 'Math/tanh :f #(Math/tanh %)}
   'floor {:sym 'Math/floor :f #(Math/floor %)}
   'ceiling {:sym 'Math/ceil :f #(Math/ceil %)}
   'modulo {:sym 'clojure.core/mod :f mod}
   'remainder {:sym 'clojure.core/rem :f rem}
   'quotient {:sym 'clojure.core/quot :f quot}
   'integer-part #?(:clj {:sym 'long :f long}
                    :cljs {:sym 'Math/trunc
                           :f #(Math/trunc %)})
   ;; NOTE that the proper way to handle this substitution is to add a
   ;; simplification rule that does this transformation for us. If you hit this
   ;; and it's slow, consider opening a PR for that.
   'fractional-part {:sym `(fn [^double ~'x]
                             (- ~'x (~'Math/floor ~'x)))
                     :fn (fn [^double x]
                           (- x (Math/floor x)))}
   #?@(:cljs
       ;; JS-only entries.
       ['acosh {:sym 'Math/acosh :f #(Math/acosh %)}
        'asinh {:sym 'Math/asinh :f #(Math/asinh %)}
        'atanh {:sym 'Math/atanh :f #(Math/atanh %)}])})

(def ^{:private true
       :doc "Dict of `<symbol> -> <symbolic-fn>`. See [[compiled-fn-whitelist]]
       for more detail."}
  sym->resolved-form
  (u/map-vals :sym compiled-fn-whitelist))

;; ## Subexpression Elimination
;;
;; This section implements subexpression extraction and "elimination", the
;; process we use to avoid redundant computation inside of a simplified function
;; body.
;;
;; The goal of this process is to split some symbolic expression into:
;;
;; - a map of symbol -> redundant subexpression
;; - a new expression with each redundant subexpr replaced with its
;;   corresponding symbol.
;;
;; The invariant we want to achieve is that the new expression, rehydrated using
;; the symbol->subexpression map, should be equivalent to the old expression.
;;
;; First, we write a function to determine how often each subexpression appears.
;; Subexpressions that appear just once aren't worth extracting, but two
;; appearances is enough.

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

#_
(+ (* (sin x) (cos x))
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

#_
[(+ g3 g3 g3) ([g1 (sin x)] [g2 (cos x)] [g3 (* g1 g2)])]

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

  ## Optional Arguments TODO say `see blah for options`

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

(def ^{:private true
       :doc "Similar to [[compiled-fn-whitelist]], but restricted to numeric
  operations."}
  numeric-whitelist
  (dissoc compiled-fn-whitelist 'up 'down))

(defn ^:no-doc apply-numeric-ops
  "Takes a function body and returns a new body with all numeric operations
  like `(/ 1 2)` evaluated and all numerical literals converted to `double` or
  `js/Number`."
  ([body]
   (w/postwalk
    (fn [expr]
      (cond (v/real? expr) (u/double expr)
            (sequential? expr)
            (let [[f & xs] expr]
              (if-let [m (and (every? number? xs)
                              (numeric-whitelist f))]
                (u/double (apply (:f m) xs))
                expr))
            :else expr))
    body)))

;; ### SCI vs Native Compilation
;;
;; Armed with the above compiler optimization we can move on to the actual
;; compilation step.
;;
;; This library provides two compilation modes:
;;
;; - Native compilation via `eval`
;; - interpreted compilation via [SCI](https://github.com/borkdude/sci), the
;;   Small Clojure Interpreter.
;;
;; We default to SCI mode in CLJS, but :native in Clojure for performance.

(def ^{:dynamic true
       :no-doc true}
  *mode*
  #?(:clj :native
     :cljs :sci))

(def ^{:doc "Set of all supported compilation modes."}
  valid-modes
  #{:sci :native :source})

(defn validate-mode!
  "Given a keyword `mode` specifying a compilation mode, returns `mode` if valid,
  and throws otherwise."
  [mode]
  (or (valid-modes mode)
      (throw
       (ex-info
        (str "Invalid compilation mode supplied: " mode
             ". Please supply (or bind to `*mode*`) one of " valid-modes)
        {:mode       mode
         :valid-mode valid-modes}))))

(defn compiler-mode
  "Validates and returns the dynamically bound compilation [[*mode*]].
  Throws on an invalid setting."
  []
  (validate-mode! *mode*))

(defn set-compiler-mode!
  "Set the default compilation mode by supplying an entry from [[valid-modes]]."
  [mode]
  (validate-mode! mode)
  #?(:cljs (set! *mode* mode)
     :clj  (alter-var-root #'*mode* (constantly mode))))

;; Native compilation works on the JVM, and on Clojurescript if you're running
;; in a self-hosted CLJS environment. Enable this mode by wrapping your call in
;;
;; `(binding [*mode* :native] ,,,)`
;;
;; ## State Functions
;;
;; `compile.cljc` currently supports compilation of:
;;
;; - univariate functions (for use with ODEs, `definite-integral` etc)
;; - "state" functions.
;;
;; A state function is a function that takes any number of arguments and returns
;; a new function of a "structure", usually an up-or-down tuple or a Clojure
;; sequence.
;;
;; The compiled version of a state function like

#_
(fn [mass g]
  (fn [q] ,,,))

;; Has a signature like

#_
(fn [q [mass g]] ,,,)

;; IE, first the structure, then a vector of the original function's arguments.

(defn- state-argv
  "Returns the argument vector for a compiled state function, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    argument to the nested function returned by the state function
  - `opts`, a dictionary of compilation options.

  See [[compile-state-fn*]] for a description of the options accepted in
  `opts`."
  [params state-model {:keys [flatten? generic-params?]
                       :or {flatten? true
                            generic-params? true}}]
  (let [state (into [] (if flatten?
                         (into [] (flatten state-model))
                         state-model))]
    (if generic-params?
      [state (into [] params)]
      [state])))

;; The following two functions compile state functions in either native or SCI
;; mode. The primary difference is that native compilation requires us to
;; explicitly replace all instances of symbols from `compiled-fn-whitelist`
;; above with actual functions.
;;
;; SCI handles this behind its interface, and simply takes a reusable context
;; that wraps the fn replacement mapping.

(def ^{:private true
       :doc "Reuseable context for SCI compilation. Fork with `sci/fork` to
  ensure that no call to `sci/eval-*` can inject state that another call can
  see."}
  sci-context
  (sci/init
   {:classes {'Math #?(:cljs js/Math :clj java.lang.Math)}

    ;; These are the only non-Math bindings introduced by the postwalk
    ;; replacement.
    :namespaces {'emmy.structure
                 {'up struct/up
                  'down struct/down}}}))

(defn sci-eval
  "Given an unevaluated source code form `f-form` representing a function,
  evaluates `f-form` using the bindings in [[sci-context]].

  Generate these forms by setting `*mode*` to `:source`."
  [f-form]
  (sci/eval-form (sci/fork sci-context) f-form))

(defn ^:no-doc array-bindings
  "

  - `include?`: fn (or a set!) that returns true of the symbol it's passed is
    used inside of the function body (and needs to be bound), false otherwise.

  - `array-sym`: symbol that stands for the array version of `inputs`.

  - `inputs`: the structure of symbols that we're converting into a primitive
    array during compilation."
  [include? array-sym inputs]
  (into [] (comp (map-indexed
                  (fn [i v]
                    (when (include? v)
                      [v `(aget ~array-sym ~i)])))
                 cat)
        (flatten inputs)))

(comment
  ;; For example:
  (= '[t (clojure.core/aget arr 0)
       thetadot (clojure.core/aget arr 2)]
     (array-bindings '#{+ t thetadot}
                     'arr
                     '[t theta thetadot])))

;; TODO add docs for the `output` arg

(defn- compile-state->source
  "Returns a natively-evaluated Clojure function that implements `body`, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function
  - `body`: a function body making use of any symbol in the args above"
  [params state-model body output _opts]
  (let [state-sym (with-meta (gensym) {:tag 'doubles})
        param-sym (with-meta (gensym) {:tag 'doubles})
        body      (w/postwalk-replace sym->resolved-form body)
        vs        (x/variables-in body)]
    ;; TODO maybe tidy up, no empty let binding if no params etc?
    `(fn [~state-sym ~param-sym ~output]
       (let ~(array-bindings vs state-sym state-model)
         (let ~(array-bindings vs param-sym params)
           ~body)))))

(defn- compile-state->source-experimental
  "Returns a natively-evaluated Clojure function that implements `body`, given:
  - `params`: a seq of symbols equal in count to the original state function's
    args

  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function

  - `body`: a function body making use of any symbol in the args above

  - `output`:

  - `opts`: (ignored here)"
  [params state-model body output _opts]
  (let [state-sym (with-meta (gensym) {:tag 'doubles})
        param-sym (with-meta (gensym) {:tag 'doubles})
        body      (w/postwalk-replace sym->resolved-form body)
        vs        (x/variables-in body)]
    ;; TODO maybe tidy up, no empty let binding if no params etc?
    `(fn [~state-sym ~param-sym ~output]
       (let ~(into (array-bindings vs state-sym state-model)
                   (array-bindings vs param-sym params))
         ~body))))

(defn- compile-state-native
  "Returns a natively-evaluated Clojure function that implements `body`, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function
  - `body`: a function body making use of any symbol in the args above"
  [params state-model body opts]
  (eval
   (compile-state->source params state-model body opts)))

(defn- compile-state-sci
  "Returns a Clojure function evaluated using SCI. The returned fn implements
  `body`, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function
  - `body`: a function body making use of any symbol in the args above"
  ([params state-model body opts]
   (sci-eval
    (compile-state->source params state-model body opts))))

;; ### State Fn Interface
;;
;; Now we come to the final interface for state function compilation. Two
;; versions of the following function exist, one that uses the global cache
;; defined above and one that doesn't.

(defn state->argv
  "Given a (structural) initial `state` and a `gensym-fn` function from symbol =>
  generated symbol walks the structure and converts all structures to vectors
  and all non-structural elements to gensymmed symbols."
  [state gensym-fn]
  (letfn [(rec [s]
            (if (struct/structure? s)
              (mapv rec s)
              (gensym-fn 'y)))]
    (rec state)))

;; TODO -
;;
;; `:flatten?`, `:generic-params?` need to get killed in favor of
;;
;; - `:parameters`, where if these are MISSING we assume they've already been
;;    supplied, and if present, then we act like `"generic-params?`" is true.
;;
(defn flatten-structs [x]
  (letfn [(sym-struct? [expr]
            (and (sequential? expr)
                 (struct/symbol-set (first expr))))]
    (filter (complement sym-struct?)
            (rest (tree-seq sym-struct? rest x)))))

(defn ^:no-doc ->output [body output]
  (if (= `let (first body))
    `(~@(take 2 body)
      ~(->output (nth body 2) output))
    `(doto ~output
       ~@(map-indexed (fn [i x]
                        `(aset ~i ~x))
                      (flatten-structs body)))))

(defn ^:no-doc ->js-output [body output]
  (if (= `let (first body))
    `(~@(take 2 body)
      ~(->output (nth body 2) output))
    `(do
       ~@(map-indexed (fn [i x]
                        `(aset ~output ~i ~x))
                      (flatten-structs body)))))

(comment
  (= '(clojure.core/doto output
        (clojure.core/aset 0 t)
        (clojure.core/aset 1 x0)
        (clojure.core/aset 2 x1)
        (clojure.core/aset 3 v0)
        (clojure.core/aset 4 v1))
     (->output
      '(up t (up x0 x1) (up v0 v1))
      'output)))

(defn compile-state-fn*
  "Returns a compiled, simplified function with signature `(f state params)`,
  given:

  - a (possibly parametric) function `f` of state => state that can accept a
    symbolic arguments

  - `prototype`: Some structure of the same shape as the argument expected by
    the fn returned by the state function `f`. Only the shape matters; the values
    are ignored.

  - an optional argument `opts`. Options accepted are:

    - `:flatten?`: if `true` (default), the returned function will have
      signature `(f <flattened-state> [params])`. If `false`, the first arg of the
      returned function will be expected to have the same shape as `initial-state`.

    - `:parameters`; Prototype of params TODO more details

      any sequence of count equal to the number of arguments taken by `f`. The
      values are ignored. TODO EDIT:: if `true` (default), the returned function
      will take a second argument for the parameters of the state derivative and
      keep params generic. If false, the returned function will take a single state
      argument, and the supplied params will be hardcoded.

    - `:genysym-fn`:

    - `:mode`: Explicitly set the compilation mode to one of the values
      in [[valid-modes]]. Explicit alternative to dynamically binding [[*mode*]].

  The returned, compiled function expects all `Double` (or `js/Number`) for all
  state primitives. The function body is simplified and all common
  subexpressions identified during compilation are extracted and computed only
  once.

  NOTE this function uses no cache. To take advantage of the global compilation
  cache, see `compile-state-fn`."
  ([f prototype]
   (compile-state-fn* f prototype {}))
  ([f prototype {:keys [parameters
                        gensym-fn
                        mode]
                 :or {gensym-fn gensym}
                 :as opts}]
   (let [sw            (us/stopwatch)
         ;; this needs to be the actual, structural argv at this stage.
         generic-state (state->argv prototype gensym-fn)
         [params g]    (if parameters
                         (let [ps (for [_ parameters] (gensym-fn 'p))]
                           [ps (apply f ps)])
                         [nil f])
         output        (with-meta (gensym) {:tag 'doubles})
         body          (-> (g generic-state)
                           (g/simplify)
                           (v/freeze)
                           (cse-form)
                           (apply-numeric-ops)
                           (->output output))
         compiler      (case (validate-mode! (or mode *mode*))
                         :source compile-state->source
                         :native compile-state-native
                         :sci compile-state-sci)
         compiled-fn   (compiler params generic-state body output opts)]
     (log/info "compiled state function in" (us/repr sw) "with mode" *mode*)
     compiled-fn)))

;; TODO: `compile-state-fn` should be more discerning in how it caches!

(defn compile-state-fn
  "Version of [[compile-state-fn*]] memoized on the `f` parameter only.
  See that function's docs for more detail.

  NOTE that this function makes use of a global compilation cache, keyed by the
  value of `f`. Passing in the same `f` twice, even with different arguments for
  `prototype`, `opts` and different compilation modes, will return the cached
  value. See `compile-state-fn*` to avoid the cache."
  ([f prototype]
   (compile-state-fn f prototype {}))
  ([f prototype opts]
   (if-let [cached (@fn-cache f)]
     (do
       (log/info "compiled state function cache hit")
       cached)
     (let [compiled (compile-state-fn* f prototype opts)]
       (swap! fn-cache assoc f compiled)
       compiled))))

;; ## To JavaScript

(defn- make-symbol-generator [p]
  (let [i (atom 0)]
    (fn [] (symbol
           #?(:clj
              (format "%s%04x" p (swap! i inc))

              :cljs
              (let [suffix (-> (swap! i inc)
                               (.toString 16)
                               (.padStart 4 "0"))]
                (str p suffix)))))))

(def infix-set
  '#{* + - / u- =})

(def js-renames
  {'sin "Math.sin"
   'cos "Math.cos"
   'tan "Math.tan"
   'asin "Math.asin"
   'acos "Math.acos"
   'atan "Math.atan"
   'cosh "Math.cosh"
   'sinh "Math.sinh"
   'tanh "Math.tanh"
   'asinh "Math.asinh"
   'acosh "Math.acosh"
   'atanh "Math.atanh"
   'sqrt "Math.sqrt"
   'abs "Math.abs"
   'expt "Math.pow"
   'log "Math.log"
   'exp "Math.exp"
   'floor "Math.floor"
   'ceiling "Math.ceil"
   'integer-part "Math.trunc"
   'not "!"})

;; TODO pull in from render... and TODO add the `else` case in `render`.

(defn- render-infix-ratio
  "renders a pair of the form `[numerator denominator]` as a infix ratio of the
  form `num/denom`.

  If the pair contains only one entry `x`, it's coerced to `[1 x]` (and treated
  as a denominator)."
  [[num denom :as xs]]
  (let [n (count xs)]
    (cond (and (= n 1) (v/integral? num))
          (str "1/" num)

          (and (= n 2)
               (v/integral? num)
               (v/integral? denom))
          (str num "/" denom)

          :else (str num " / " denom))))

(def js-handlers
  (let [->parens #(str "(" % ")")
        ->js-vector #(str \[ (cs/join ", " %) \])]
    {'up ->js-vector
     'down ->js-vector
     'modulo (fn [[a b]]
               (-> (str a " % " b)
                   (->parens)
                   (str " + " b)
                   (->parens)
                   (str " % " b)
                   (->parens)))
     'remainder (fn [[a b]]
                  (str a " % " b))
     'and (fn [[a b]] (str a " && " b))
     'or  (fn [[a b]] (str a " || " b))

     ;; TODO this is dumb for now!!
     'do  (fn [xs] (apply str xs))
     `aset
     (fn [[a i v]] (str "  " a "[" (int i) "] = " v ";\n"))
     '/ render-infix-ratio}))

;; TODO return `(js/Function. "s" "p" "out" "body")`
(let [->parens #(str "(" % ")")]
  (defn ->js [xs]
    (str
     (w/postwalk
      (fn [expr]
        (cond (symbol? expr) expr
              (v/real? expr) (u/double expr)
              (sequential? expr)
              (let [[f & xs] expr]
                (if-let [m (and (every? number? xs)
                                (numeric-whitelist f))]
                  (u/double (apply (:f m) xs))
                  (or (when-let [f' (js-handlers f)]
                        (f' xs))

                      ;; infix
                      (when-let [sym (infix-set f)]
                        (->parens
                         (cs/join (str " " sym " ") xs)))

                      ;; prefix
                      (when-let [f-name (js-renames f)]
                        (str f-name
                             (->parens
                              (cs/join " , " xs))))

                      (u/illegal (str "Unknown op: " f)))))
              :else expr))
      xs))))

(comment
  (let [xs '(+ (* x y z)
               (- (* a b c)
                  (/ d g)))]
    (->js xs)))

;; TODO I expect that the guts here will be mostly the same...
(defn compile-js
  ([f prototype]
   (compile-js f prototype {}))
  ([f prototype {:keys [parameters
                        gensym-fn
                        mode]
                 :or {gensym-fn gensym}
                 :as opts}]
   (let [sw            (us/stopwatch)
         callback      (fn [new-expression new-vars]
                         (str (cs/join
                               (->> (for [[var val] new-vars]
                                      (str "  var " var " = " (->js val) ";\n"))
                                    (apply str)))
                              (->js new-expression) ";"))
         ;; this needs to be the actual, structural argv at this stage.
         generic-state (state->argv prototype gensym-fn)
         [params g]    (if parameters
                         (let [ps (for [_ parameters] (gensym-fn 'p))]
                           [ps (apply f ps)])
                         [nil f])
         state-sym     (with-meta (gensym) {:tag 'doubles})
         param-sym     (with-meta (gensym) {:tag 'doubles})
         output-sym    (with-meta (gensym) {:tag 'doubles})
         body (-> (g generic-state)
                  (g/simplify)
                  (v/freeze))
         vs (x/variables-in body)
         body (-> body
                  (->js-output output-sym)
                  (extract-common-subexpressions callback opts))
         s-bindings (transduce
                     (comp (map-indexed
                            (fn [i v]
                              (when (vs v)
                                [(str "  " v " = " state-sym "[" i "];\n")])))
                           cat)
                     str
                     (flatten generic-state))
         p-bindings (transduce
                     (comp (map-indexed
                            (fn [i v]
                              (when (vs v)
                                [(str "  " v " = " param-sym "[" i "];\n")])))
                           cat)
                     str
                     params)
         body-str (str s-bindings p-bindings body)]
     (log/info "compiled state function in" (us/repr sw) "with mode :js")
     #_(js/console.log
        (str state-sym)
        (str param-sym)
        (str output-sym)
        body-str)
     #?(:clj
        {:params [(str state-sym)
                  (str param-sym)
                  (str output-sym)]
         :body body-str}
        :cljs
        (js/Function.
         (str state-sym)
         (str param-sym)
         (str output-sym)
         body-str)))))

;; ## Non-State-Functions
;;
;; Compiled functions are excellent input for `definite-integral`, ODE solvers,
;; single variable function minimization, root finding and more.
;;
;; The implementation and compilation steps are simpler than the state function
;; versions above; the function you pass in has to take `n` symbolic arguments,
;; that's it.

(defn- compile->source
  "Returns an unevaluated source code body function that implements `body`, given
  some sequence `args` of argument symbols.

  `body` should of course make use of the symbols in `args`."
  [args body]
  (let [body (w/postwalk-replace sym->resolved-form body)]
    `(fn [~@args] ~body)))

(defn- compile-native
  "Returns a natively-evaluated Clojure function that implements `body`, given
  some sequence `args` of argument symbols.

  `body` should of course make use of the symbols in `args`."
  [args body]
  (eval
   (compile->source args body)))

(defn- compile-sci
  "Returns a Clojure function evaluated
  using [SCI](https://github.com/borkdude/sci) The returned fn implements
  `body`, given some sequence `args` of argument symbols.

  `body` should of course make use of the symbols in `args`."
  [args body]
  (sci-eval
   (compile->source args body)))

(defn- retrieve-arity [f]
  (let [[kwd n :as arity] (f/arity f)]
    (if (= kwd :exactly)
      n
      (u/illegal
       (str "`compile-fn` can only infer arity for functions with just one
           arity, not " arity ". Please pass an explicit `n`.")))))

(defn compile-fn*
  "Returns a compiled, simplified version of `f`, given a function `f` of arity
  `n` (ie, able to accept `n` symbolic arguments).

  `n` defaults to `([[f/arity]] f)`.

  The returned, compiled function expects `n` `Double` (or `js/Number`)
  arguments. The function body is simplified and all common subexpressions
  identified during compilation are extracted and computed only once.

  NOTE: this function uses no cache. To take advantage of the global compilation
  cache, see `compile-fn`."
  ([f] (compile-fn* f (retrieve-arity f)))
  ([f n]
   (let [sw       (us/stopwatch)
         args     (repeatedly n #(gensym 'x))
         body     (-> (apply f args)
                      (g/simplify)
                      (v/freeze)
                      (cse-form)
                      (apply-numeric-ops))
         compiled (case (compiler-mode)
                    :source (compile->source args body)
                    :native (compile-native args body)
                    :sci (compile-sci args body))]
     (log/info "compiled function of arity" n "in" (us/repr sw) "with mode" *mode*)
     compiled)))

(defn compile-fn
  "Memoized version of [[compile-fn*]]. See that function's docs for more detail.

  NOTE: that this function makes use of a global compilation cache, keyed by the
  vector `[f n *mode*]`. See `compile-fn*` to avoid the cache."
  ([f] (let [[kwd n :as arity] (f/arity f)]
         (when-not (= kwd :exactly)
           (u/illegal
            (str "`compile-fn` can only infer arity for functions with just one
           arity, not " arity ". Please pass an explicit `n`.")))
         (compile-fn f n)))
  ([f n]
   (let [mode *mode*]
     (if-let [cached (@fn-cache [f n mode])]
       (do
         (log/info "compiled function cache hit - arity " n ", mode " mode)
         cached)
       (binding [*mode* mode]
         (let [compiled (compile-fn* f n)]
           (swap! fn-cache assoc [f n mode] compiled)
           compiled))))))
