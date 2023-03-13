#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.compile
  "This namespace contains tools for compiling functions implemented with the
  numeric operations defined in [[emmy.generic]] down to fast, native
  functions."
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [emmy.expression.analyze :as a]
            [emmy.expression.cse :refer [extract-common-subexpressions]]
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

(def ^:private compiled-fn-whitelist
  "Dict of `<symbol> -> {:sym <symbolic-fn>, :f <evaluated-fn>}`. The keys
  constitute the set of operations allowed to appear within the body of the
  compiled function.

  If you're compiling a function for use with the numerical routines, the
  library assumes that your function operates only on doubles (even though you
  wrote it with generic routines)."
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

(def ^:private sym->resolved-form
  "Dict of `<symbol> -> <symbolic-fn>`. See [[compiled-fn-whitelist]] for more
  detail."
  (u/map-vals :sym compiled-fn-whitelist))

(def ^:private numeric-whitelist
  "Similar to [[compiled-fn-whitelist]], but restricted to numeric operations."
  (dissoc compiled-fn-whitelist 'up 'down))

(defn ^:no-doc apply-numeric-ops
  "Takes a function body and returns a new body with all numeric operations
  like `(/ 1 2)` evaluated and all numerical literals converted to `double` or
  `js/Number`."
  [body]
  (w/postwalk
   (fn [expr]
     (cond
       (v/real? expr) (u/double expr)
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

(def valid-modes
  "Set of all supported compilation modes."
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

(defn-
  primitive-state-symbols
  "Generates compiled function argument symbols for input and output state and
  parameters when compiling in primitive mode. A type hint is attached to
  the symbols to avoid reflection."
  [gensym]
  (mapv #(with-meta % {:tag 'doubles}) (repeatedly 3 gensym)))

;; Native compilation works on the JVM and in ClojureScript. Enable this mode
;; by wrapping your call in
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

#_(fn [mass g]
    (fn [q]))

;; Has a signature like

#_(fn [q [mass g]])

;; I.E., first the structure, then a vector of the original function's arguments.

(defn- state-argv
  "Computes the argument list corresponding to `state-model` according to
  the `:calling-convention` in the code object. The argument list will be
  placed stored in the `:argv` key, and the original state model itself
  in `:model`.

  - `state-model`: a structure of symbols representing the state value
    operated on by the state derivative

  Effectively, there are four classes of signature handled here.

  In each of the following displays, the parameter argument list
  `[p0...]` will be present only if the key `:params` is present
  on the code object, corresponding to the use of the
  `:generic-params?` compilation option.

  - `:structure => (fn [[t [x0 x1] [v0 v1]] [p0 p1 ...]] ...) `

  This is the typical signature for a state derivative discussed in
  SICM. It maps a state tuple to its time derivative; structure in,
  structure out.

  - `:flat => (fn [[t x0 x1 v0 v1] [p0 p1 ...]] ...)`

  ODE solvers prefer to work with flat vectors, and so this calling
  convention unrolls the state model into an ordered list of parameters.

  - `:primitive => (fn [ys yps ps])`

  ODE solvers can gain yet more efficiency if they may allocate
  long-lived input and output vectors. Bindings for the individual
  state elements are inserted into the code's local variables.
  The input vector `ys` should have the same length as the flat
  form of `state-model`. The derivative should be returned by
  mutating the array `yps`. The parameters `ps` will also be a
  primitive array, which (like `ys`) should be considered read-only.

  = `:native => (fn [t [x0 x1] [v0 v1]] ...)

  It is useful to compile functions with arbitrary argument structure
  too. The native convention allows you to get the same argument list
  that you provide to `fn` to create the function. This is convenient
  for use with univariate integration libraries."
  [{:keys [calling-convention params] :as code} gensym-fn state-model]
  (let [argv (case calling-convention
               :primitive (primitive-state-symbols #(gensym-fn "a"))
               :structure [(into [] state-model)]
               :flat [(into [] (flatten state-model))]
               :native state-model
               (throw (ex-info "Invalid calling convention supplied"
                               {:calling-convention calling-convention})))]
    (assoc code
           :model state-model
           :argv (if (and params (not= calling-convention :primitive))
                   (conj argv (into [] params))
                   argv))))

(defn- primitive-bindings
  [{:keys [argv target calling-convention model params] :as code}]
  (letfn [(local-vars-from-array
           [array-symbol vars]
           (map-indexed (fn [i v]
                          [v (case target
                               :clj `(aget ~array-symbol ~i)
                               :js (str array-symbol "[" i "]"))])
                        vars))]
    (case calling-convention
     :primitive (update code :locals concat
                        (local-vars-from-array (first argv) (flatten model))
                        (local-vars-from-array (nth argv 2) params))
     code)))

;; The following functions compile state functions in either native or SCI
;; mode. The primary difference is that native compilation requires us to
;; explicitly replace all instances of symbols from `compiled-fn-whitelist`
;; above with actual functions.
;;
;; SCI handles this behind its interface, and simply takes a reusable context
;; that wraps the fn replacement mapping.

(def ^:private sci-context
  "Reuseable context for SCI compilation. Fork with `sci/fork` to ensure that no
  call to `sci/eval-*` can inject state that another call can see."
  (sci/init
   {:classes {'Math #?(:cljs js/Math :clj java.lang.Math)}

    ;; These are the only non-Math bindings introduced by the postwalk
    ;; replacement.
    :namespaces {'emmy.structure
                 {'up struct/up
                  'down struct/down}}}))

(defn- commafy-arglist
  "Since JavaScript likes commas between array elements:

   [a [b c [d e] f]] -> \"[a, [b, c, [d, e], f]]\"

   However, [] (which occurs in parameter-free state functions)
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
                 (let [as (s/join ", " f)]
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

  - `argv`: a vector of symbols to serve as the function's arguments.
    The vector may be nested for sequence destructuring.

  - `body`: a function body making use of any symbol in argv above"
  [{:keys [argv locals body]}]
  (w/postwalk-replace
   sym->resolved-form
   (if (seq locals)
     `(fn [~@argv]
        (let ~(into [] cat locals)
          ~body))
     `(fn [~@argv]
        ~body))))

(defn- compile->js
  "Returns an array containing JavaScript source for a function that implements
  its code object argument in a form suitable for application of the Function
  constructor."
  [{:keys [argv locals body]}]
  (let [argv        (map commafy-arglist argv)
        code        (s/join
                     (concat
                      (for [[var val] locals]
                        (str "  const " var " = " (render/->JavaScript val) ";\n"))
                      ["  return " (render/->JavaScript body) ";"]))]
    (into [] (concat argv [code]))))

(defn- compile-native
  "Dispatches the `code` object to the compiler corresponding to the
  current runtime environment."
  [code]
  #?(:clj (eval (compile->clj code))
     :cljs (comp js->clj (apply js/Function (compile->js code)))))

(defn- compile-sci
  "Returns a Clojure function evaluated using SCI. The returned fn implements
  `code`, given:

 - `code`: a code object, which we hand off to the Clojure compiler"
  ([code]
   (sci-eval
    (compile->clj code))))

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

(defn- cse
  "Invokes [[emmy.expression.cse/extract-common-subexpressions]] on `x`.

  Local bindings for the common subexpressions found are appended to the
  `:locals` field of the code object, and the (possibly) simplified
  code replaces that int the `:body` field."
  [x gensym-fn deterministic?]
  (extract-common-subexpressions
   (:body x)
   (fn [new-body new-locals]
     (-> x
         (update :locals into new-locals)
         (assoc :body new-body)))
   {:gensym-fn gensym-fn :deterministic? deterministic?}))

(defn- constant-fold
  "Applies a constant-folding pass to the local expressions and the body
   of a code object."
  [code]
  (-> code
      (update :locals #(mapv (fn [[k v]] [k (apply-numeric-ops v)]) %))
      (update :body apply-numeric-ops)))

(defn compile-state-fn*
  "Returns a compiled, simplified function with signature `(f state params)`,
  given:

  - a state function that can accept a symbolic arguments

  - `params`; really any sequence of count equal to the number of arguments
    taken by `f`. The values are ignored. If the specific value `false` is
    provided, then `f` is considered to be the function to compile itself,
    and not the producer of such a function via application of parameters.

  - `initial-state`: Some structure of the same shape as the argument expected
    by the fn returned by the state function `f`. Only the shape matters; the
    values are ignored.

  - an optional argument `opts`. Options accepted are:

    - `:calling-convention`: May have one of the following values. (In
      each of these examples, assume that the initial state
      `(up 1 (up 2 3) (up 3 4)) has been provided.)

      - `:structure`: The arguments to the compiled function will have
        the same shape as the initial-state, and elements of that state
        will be made available to the function via argument destructuring
        in function signature, e.g.:

        ```clojure
        (fn [[y1 [y2 y3] [y4 y5]]] [p1 ...] ...)
        ```

      - `:flat`: The compiled function will expect the state in flattend
        form, which may be provided by any flat Clojure sequence:

        ```clojure
        (fn [[y1 y2 y3 y4 y5] [p1 ...]] ...)
        ```

      - `:primitive`: The compiled function will expect a primitve array
        containing the state in flat form to be passed as the first
        argument, and will return its value by mutating its second argument,
        which will also be a primitive array of the same size. The parameters
        will be provided via a third primitive array:

        ```clojure
        (fn [ys yps ps] ...)
        ```

        This is the fastest form, as no allocations are needed to destructure
        arguments list or to construct the return value, but requires the use
        of primitive arrays (not general Clojure sequences, even if mutable) by
        the caller. The generated code will use `aget` and `aset` on the arrays.

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
  ([f params initial-state {:keys [mode
                                   calling-convention
                                   generic-params?
                                   gensym-fn
                                   deterministic?]
                            :or {mode *mode*
                                 calling-convention :structure
                                 generic-params? true
                                 gensym-fn (a/monotonic-symbol-generator 4)
                                 deterministic? false}}]
   (let [sw            (us/stopwatch)
         mode          (validate-mode! mode)
         target        (case mode
                         (:clj :sci) :clj
                         (:source :native) #?(:clj :clj :cljs :js)
                         :js :js)
         wrap          (fn [code & {:as opts}] (merge {:body code :locals []} opts))
         generic-state (state->argv initial-state gensym-fn)
         params        (and params
                            (if generic-params?
                              (for [_ params] (gensym-fn 'p))
                              params))
         g             (if-not (false? params) (apply f params) f)
         h             (case calling-convention
                         :native (apply g generic-state)
                         (g generic-state))
         _             (println "generic-state" generic-state "initial-state" initial-state)
         code          (-> h
                           (g/simplify)
                           (v/freeze)
                           (wrap :target target
                                 :calling-convention calling-convention
                                 :params (when generic-params? params))
                           (cse #(gensym-fn "_") deterministic?)
                           (state-argv gensym-fn generic-state)
                           (constant-fold)
                           (primitive-bindings))
         compiler      (case mode
                         :source #?(:clj compile->clj :cljs compile->js)
                         :clj compile->clj
                         :js compile->js
                         :native compile-native
                         :sci compile-sci)
         compiled-fn   (compiler code)]
     (log/info "compiled function in" (us/repr sw))
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
  `n` (i.e., able to accept `n` symbolic arguments).

  `n` defaults to `([[f/arity]] f)`.

  The returned, compiled function expects `n` `Double` (or `js/Number`)
  arguments. The function body is simplified and all common subexpressions
  identified during compilation are extracted and computed only once.

  NOTE: this function uses no cache. To take advantage of the global compilation
  cache, see `compile-fn`."
  ([f] (compile-fn* f (retrieve-arity f)))
  ([f n]
   (let [argv     (into [] (repeatedly n #(gensym 'x)))]
     (compile-state-fn* f false argv {:calling-convention :native}))))

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

(comment
  (compile-fn* g/+ 2)
  ((compile-fn* g/+ 2) 6 7)
  ((compile-fn* g/+ 3) 'y 'x 7)
  (def c (compile-state-fn* g/+ false ['x 'y 'z]
                            {:calling-convention :native
                             :mode :native}))

  c

  (c 1 2 4)
  )
