#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.compile
  "This namespace contains tools for compiling functions implemented with the
  numeric operations defined in [[emmy.generic]] down to fast, native
  functions."
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [emmy.expression.cse :refer [cse-form]]
            [emmy.expression.render :as render]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.structure :as struct]
            [emmy.util :as u]
            [emmy.util.stopwatch :as us]
            [emmy.value :as v]
            [sci.core :as sci]
            [taoensso.timbre :as log]))

;; # Function Compilation
;;
;; Functions built out of generic operations can be compiled down into fast,
;; efficient functions that operate on doubles. The process is:
;;
;; 1. Pass symbols like `'x` in for all arguments. This will cause the function
;;    to return a "numerical expression", a syntax tree representing the
;;    function's body:
#_(let [f (fn [x] (g/sqrt
                   (g/+ (g/square (g/sin x))
                        (g/square (g/cos x)))))]
    (= '(sqrt (+ (expt (sin x) 2) (expt (cos x) 2)))
       (x/expression-of (f 'x))))

;; 2. `g/simplify` the new function body. Sometimes this results in large
;;    simplifications:

#_(let [f (fn [x] (g/sqrt
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
   'down {:sym 'emmy.structure/down
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

(def ^{:private true
       :doc "Similar to [[compiled-fn-whitelist]], but restricted to numeric
  operations."}
  numeric-whitelist
  (dissoc compiled-fn-whitelist 'up 'down))

(defn ^:no-doc apply-numeric-ops
  "Takes a function body and returns a new body with all numeric operations
  like `(/ 1 2)` evaluated and all numerical literals converted to `double` or
  `js/Number`."
  [body]
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
   body))

;; ### SCI vs Native Compilation
;;
;; Armed with the above compiler optimization we can move on to the actual
;; compilation step.
;;
;; This library provides several compilation modes:
;;
;; - Native compilation via `eval` in Clojure, transpiling in JavaScript
;; - interpreted compilation via [SCI](https://github.com/borkdude/sci), the
;;   Small Clojure Interpreter.
;; - Clojure source
;; - JavaScript source
;; - just asking for :source will provide the source in whichever environment
;;   you're using
;;
;; We default to :native for performance.

(def ^{:dynamic true
       :no-doc true}
  *mode*
  :native)

(def ^{:doc "Set of all supported compilation modes."}
  valid-modes
  #{:sci :native :source :js :clj})

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

;; Native compilation works on the JVM and in ClojureScript. Enable this mode
;; by wrapping your call in
;;
;; (binding [*mode* :native] ,,,)`
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

#_(fn [mass g]
    (fn [q]))

;; Has a signature like

#_(fn [q [mass g]])

;; IE, first the structure, then a vector of the original function's arguments.

(defn- state-argv
  "Returns the argument vector for a compiled state function, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function
  - `opts`, a dictionary of compilation options.

  See [[compile-state-fn*]] for a description of the options accepted in
  `opts`."
  [params state-model {:keys [flatten? generic-params?]
                       :or {flatten? true
                            generic-params? true}}]
  (let [state (into [] (if flatten?
                         (flatten state-model)
                         state-model))]
    (if generic-params?
      [state (into [] params)]
      [state])))

;; The following functions compile state functions in either native or SCI
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

(defn- commafy-arglist
  "Since JavaScript likes commas between array elements:

   [a [b c [d e] f]] -> \"[a, b, c, d, e, f]\"       if flatten?
   [a [b c [d e] f]] -> \"[a, [b, c, [d, e], f]]\"   if not flatten?

   However, [] (which occurs in parameterless state functions)
   will appear as \"_\" in the argument list. While it's not
   wrong to destructure an empty list in JavaScript, nil is
   not iterable, and so can't serve as an empty list argument
   as it can in Clojure. Instead, we \"discard\" the argument
   by letting it bind to a dummy name: in this way, either nil
   or [] can serve as the empty argument"
  [a]
  (let [c (w/postwalk
           (fn [f]
             (if (sequential? f)
               (if (seq f)
                 (let [as (clojure.string/join ", " f)]
                             (str "[" as "]"))
                 "_")
               (str f)))
           a)]
    c))

(defn sci-eval
  "Given an unevaluated source code form `f-form` representing a function,
  evaluates `f-form` using the bindings in [[sci-context]].

  Generate these forms by setting `*mode*` to `:source`."
  [f-form]
  (sci/eval-form (sci/fork sci-context) f-form))

(defn- compile->clj
  "Returns Clojure source for a function that implements `body`, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function
  - `body`: a function body making use of any symbol in the args above"
  [argv body]
  (let [body (w/postwalk-replace sym->resolved-form body)]
    `(fn [~@argv] ~body)))

(defn- compile->js
  "Returns an array containing JavaScript source for a function that implements
  `body` in a form suitable for the arguments of the Function constructor, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function
  - `body`: a function body making use of any symbol in the args above"
  [argv body]
  (let [argv        (mapv commafy-arglist argv)
        js          (render/->JavaScript* body)
        cs-bindings (s/join (for [[var val] (:vars js)]
                              (str "  const " var " = " val ";\n")))
        body        (str cs-bindings
                         "  return "
                         (:value js)
                         ";")]
    (conj argv body)))

(defn- compile-native
  "Returns a natively-evaluated function that implements `body`, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function
  - `body`: a function body making use of any symbol in the args above"
  [argv body]
  #?(:clj (eval (compile->clj argv body))
     :cljs (comp js->clj (apply js/Function (compile->js argv body)))))

(defn- compile-sci
  "Returns a Clojure function evaluated using SCI. The returned fn implements
  `body`, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function
  - `body`: a function body making use of any symbol in the args above"
  ([argv body]
   (sci-eval
    (compile->clj argv body))))

;; ### State Fn Interface
;;
;; Now we come to the final interface for state function compilation. Two
;; versions of the following function exist, one that uses the global cache
;; defined above and one that doesn't.

(defn- state->argv
  "Given a (structural) initial `state` and a `gensym-fn` function from symbol =>
  generated symbol walks the structure and converts all structures to vectors
  and all non-structural elements to gensymmed symbols."
  [state gensym-fn]
  (letfn [(rec [s]
            (if (struct/structure? s)
              (mapv rec s)
              (gensym-fn 'y)))]
    (rec state)))

(defn compile-state-fn*
  "Returns a compiled, simplified function with signature `(f state params)`,
  given:

  - a state function that can accept a symbolic arguments

  - `params`; really any sequence of count equal to the number of arguments
    taken by `f`. The values are ignored.

  - `initial-state`: Some structure of the same shape as the argument expected
    by the fn returned by the state function `f`. Only the shape matters; the
    values are ignored.

  - an optional argument `opts`. Options accepted are:

    - `:flatten?`: if `true` (default), the returned function will have
      signature `(f <flattened-state> [params])`. If `false`, the first arg of the
      returned function will be expected to have the same shape as `initial-state`

    - `:generic-params?`: if `true` (default), the returned function will take a
      second argument for the parameters of the state derivative and keep params
      generic. If false, the returned function will take a single state argument,
      and the supplied params will be hardcoded.

    - `:mode`: Explicitly set the compilation mode to one of the values
      in [[valid-modes]]. Explicit alternative to dynamically binding [[*mode*]].

  The returned, compiled function expects all `Double` (or `js/Number`) for all
  state primitives. The function body is simplified and all common
  subexpressions identified during compilation are extracted and computed only
  once.

  NOTE this function uses no cache. To take advantage of the global compilation
  cache, see `compile-state-fn`."
  ([f params initial-state]
   (compile-state-fn* f params initial-state {}))
  ([f params initial-state {:keys [generic-params?
                                   gensym-fn
                                   mode]
                            :or {generic-params? true
                                 gensym-fn gensym}
                            :as opts}]
   (let [sw            (us/stopwatch)
         params        (if generic-params?
                         (for [_ params] (gensym-fn 'p))
                         params)
         generic-state (state->argv initial-state gensym-fn)
         g             (apply f params)
         body          (-> (g generic-state)
                           (g/simplify)
                           (v/freeze)
                           #?(:clj (cse-form))  ;; JavaScript handles this in render
                           (apply-numeric-ops))
         compiler      (case (validate-mode! (or mode *mode*))
                         :source #?(:clj compile->clj :cljs compile->js)
                         :clj compile->clj
                         :js compile->js
                         :native compile-native
                         :sci compile-sci)
         argv          (state-argv params generic-state opts)
         compiled-fn   (compiler argv body)]
     (log/info "compiled state function in" (us/repr sw) "with mode" *mode* "and flatten" (:flatten? opts) "yielding" compiled-fn)
     compiled-fn)))

;; TODO: `compile-state-fn` should be more discerning in how it caches!

(defn compile-state-fn
  "Version of [[compile-state-fn*]] memoized on the `f` parameter only.
  See that function's docs for more detail.

  NOTE that this function makes use of a global compilation cache, keyed by the
  value of `f`. Passing in the same `f` twice, even with different arguments for
  `param` and `initial-state` and different compilation modes, will return the
  cached value. See `compile-state-fn*` to avoid the cache."
  ([f params initial-state]
   (compile-state-fn f params initial-state {}))
  ([f params initial-state opts]
   (if-let [cached (@fn-cache f)]
     (do
       (log/info "compiled state function cache hit")
       cached)
     (let [compiled (compile-state-fn* f params initial-state opts)]
       (swap! fn-cache assoc f compiled)
       compiled))))

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
                    :source (#?(:clj compile->clj :cljs compile->js) args body)
                    :js (compile->js args body)
                    :clj (compile->clj args body)
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
