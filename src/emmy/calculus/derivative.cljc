#_"SPDX-License-Identifier: GPL-3.0"

^#:nextjournal.clerk
{:toc true
 :visibility :hide-ns}
(ns emmy.calculus.derivative
  "This namespace implements a number of differential operators like [[D]], and
  the machinery to apply [[D]] to various structures."
  (:refer-clojure :exclude [partial])
  (:require [emmy.autodiff]
            [emmy.dual :as d]
            [emmy.expression :as x]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.matrix :as matrix]
            [emmy.operator :as o]
            [emmy.series :as series]
            [emmy.structure :as s]
            [emmy.tape :as tape]
            [emmy.util :as u]
            [emmy.value :as v]))

;; ## Single and Multivariable Calculus
;;
;; These functions put together the pieces laid out in [[emmy.dual]] and declare
;; an interface for taking derivatives.

;; The result of applying the derivative `(D f)` of a multivariable function `f`
;; to a sequence of `args` is a structure of the same shape as `args` with all
;; orientations flipped. (For a partial derivative like `((partial 0 1) f)` the
;; result has the same-but-flipped shape as `(get-in args [0 1])`.)
;;
;; `args` is coerced into an `up` structure. The only special case where this
;; does not happen is if `(= 1 (count args))`.
;;
;; To generate the result:
;;
;; - For a single non-structural argument, return `(d/derivative f)`
;; - else, bundle up all arguments into a single [[emmy.structure/Structure]]
;;   instance `xs`
;; - Generate `xs'` by replacing each entry in `xs` with `((d/derivative f')
;;   entry)`, where `f'` is a function of ONLY that entry that
;;   calls `(f (assoc-in xs path entry))`. In other words, replace each entry
;;   with the result of the partial derivative of `f` at only that entry.
;; - Return `(s/transpose xs')` (the same structure with all orientations
;;   flipped.)
;;
;; A multivariable derivative is a multiple-arity function that performs the
;; above.
;;
;; [[jacobian]] handles this main logic. [[jacobian]] can only take a structural
;; input. [[euclidean]] and [[multivariate]] below handle, respectively,
;; optionally-structural and multivariable arguments.

(defn- deep-partial
  "Returns the partial derivative of `f` with respect to the entry in `structure`
  at the location `path`.

  `entry` defaults to `(get-in structure path)`."
  ([f structure path]
   (let [entry (get-in structure path)]
     (deep-partial f structure path entry)))
  ([f structure path entry]
   (if (v/scalar? entry)
     (letfn [(f-entry [x]
               (f (assoc-in structure path x)))]
       ((d/derivative f-entry) entry))
     (u/illegal
      (str "non-numerical entry " entry
           " at path " path
           " in input structure " structure)))))

(defn- jacobian
  "Takes:

  - some function `f` of a single [[emmy.structure/structure?]] argument
  - the unperturbed structural `input`
  - a `selectors` vector that can be empty or contain a valid path into the
    `input` structure

  and returns either:

  - The full [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
    of `f` at `input`, if `selectors` is empty
  - the entry of the Jacobian at `selectors`

  The Jacobian has the same shape as `input` (or the entry at `selectors`) with
  all orientations flipped. Multiply this by an increment in the shape of
  `input` to produce an increment in the output of `f`."
  ([f input] (jacobian f input []))
  ([f input selectors]
   (letfn [(prefixed [path]
             (if (empty? selectors)
               path
               (into selectors path)))]
     (if-let [piece (get-in input selectors)]
       (let [frame (s/transpose piece)]
         ;; Visit each entry in `frame`, a copy of either the full input or the
         ;; sub-piece living at `selectors` (with all orientations flipped), and
         ;; replace the entry with the result of the partial derivative of `f`
         ;; with that entry perturbed.
         (s/map-chain
          (fn [entry path _]
            (deep-partial f input (prefixed path) entry))
          frame))

       ;; The call to `get-in` will return nil if the `selectors` don't index
       ;; correctly into the supplied `input`, triggering this exception.
       (u/illegal (str "Bad selectors " selectors " for structure " input))))))

(defn- euclidean
  "Slightly more general version of [[jacobian]] that can handle a single input;
  dispatches to either [[jacobian]] or [[derivative]] depending on whether or
  not the input is structural.

  If you pass non-empty `selectors`, the returned function will throw if it
  receives a non-structural, non-scalar argument."
  ([f] (euclidean f []))
  ([f selectors]
   (let [selectors (vec selectors)]
     (fn [input]
       (cond (s/structure? input)
             (jacobian f input selectors)

             ;; non-empty selectors are only allowed for functions that receive
             ;; a structural argument. This case passes that single,
             ;; non-structural argument on to `(d/derivative f)`.
             (empty? selectors)
             ((d/derivative f) input)

             ;; Any attempt to index (via non-empty selectors) into a
             ;; non-structural argument will throw.
             ;;
             ;; NOTE: What about matrices, maps or sequences? The current
             ;; implementation (as of 0.15.0) pushes the derivative operator
             ;; into the entries, or values, of those types, so they won't reach
             ;; this clause. There is a case I (@sritchie) can make for actually
             ;; allowing the first clause here to work for ANY associative
             ;; structure; then you're on your own if you want to call this fn
             ;; directly.
             :else
             (u/illegal
              (str "Selectors " selectors
                   " not allowed for non-structural input " input)))))))

(defn- multi
  "Given

    - some higher-order function `op` that transforms a function of a single
      variable into another function of a single variable
    - function `f` capable of taking multiple arguments

  returns a new function that acts like `(op f)` but can take multiple
  arguments.

  When passed multiple arguments, the returned functon packages them into a
  single `[[emmy.structure/up]]` instance. Any [[emmy.matrix/Matrix]] present in
  the argument list will be converted into a `down` of `up`s (a row of columns)."
  [op f]
  (-> (fn
        ([] 0)
        ([x] ((op f) x))
        ([x & more]
         ((multi op (fn [xs] (apply f xs)))
          (matrix/seq-> (cons x more)))))
      (f/with-arity (f/arity f) {:from ::multi})))

(defn- multivariate
  "Slightly wider version of [[euclidean]]. Accepts:

  - some function `f` of potentially many arguments
  - optionally, a sequence of selectors meant to index into the structural
    argument, or argument vector, of `f`

  And returns a new function that computes either the
  full [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
  or the entry at `selectors` using [forward-mode automatic
  differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation#Forward_accumulation).

  Any multivariable function will have its argument vector coerced into an `up`
  structure. Any [[emmy.matrix/Matrix]] in a multiple-arg function call will be
  converted into a `down` of `up`s (a row of columns).

  Arguments to single-variable functions are not transformed."
  ([f] (multivariate f []))
  ([f selectors]
   (let [d #(euclidean % selectors)]
     (multi d f))))

(defn gradient
  "Accepts:

  - some function `f` of potentially many arguments
  - optionally, a sequence of selectors meant to index into the structural
    argument, or argument vector, of `f`

  And returns a new function that computes either the
  full [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
  or the entry at `selectors` using [reverse-mode automatic
  differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation#Reverse_accumulation).

  Any multivariable function will have its argument vector coerced into an `up`
  structure. Any [[emmy.matrix/Matrix]] in a multiple-arg function call will be
  converted into a `down` of `up`s (a row of columns).

  Arguments to single-variable functions are not transformed."
  ([f] (gradient f []))
  ([f selectors]
   (multi #(tape/gradient % selectors) f)))

;; ## Generic [[g/partial-derivative]] Installation
;;
;; [[g/partial-derivative]] is meant to produce either a full Jacobian or some
;; entry specified by a `selectors` vector.
;;
;; When called on a function `f`, [[g/partial-derivative]] returns a function
;; wrapped in the machinery provided by [[multivariate]]; this allows the same
;; operator to serve functions of:
;;
;; - a single numerical input
;; - a single structural input
;; - multiple numerical OR structural inputs
;;
;; NOTE: The reason that this implementation is also installed
;; for [[emmy.structure/Structure]] is that structures act as functions
;; that apply their args to every (functional) entry. Calling `(multivariate
;; structure selectors)` allows all of the machinery that handles
;; structure-walking and argument conversion to run a SINGLE time before getting
;; passed to the structure of functions, instead of separately for every entry
;; in the structure.
;;
;; A dynamic variable controls whether or not this process uses forward-mode or
;; reverse-mode AD.
;;
;; TODO: I think this is going to cause problems for, say, a Structure of
;; PowerSeries, where there is actually a cheap `g/partial-derivative`
;; implementation for the components. I vote to back out this `::s/structure`
;; installation.

(def ^:dynamic *mode* d/REVERSE-MODE)

(doseq [t [::v/function ::s/structure]]
  (defmethod g/partial-derivative [t v/seqtype] [f selectors]
    (if (= *mode* d/FORWARD-MODE)
      (multivariate f selectors)
      (gradient f selectors)))

  (defmethod g/partial-derivative [t nil] [f _]
    (if (= *mode* d/FORWARD-MODE)
      (multivariate f [])
      (gradient f []))))

;; ## Operators
;;
;; This section exposes various differential operators
;; as [[emmy.operator/Operator]] instances.

(def ^{:arglists '([f])}
  D-forward
  "Forward-mode derivative operator. Takes some function `f` and returns a
  function whose value at some point can multiply an increment in the arguments
  to produce the best linear estimate of the increment in the function value.

  For univariate functions, [[D-forward]] computes a derivative. For vector-valued
  functions, [[D-forward]] computes
  the [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
  of `f`."
  (o/make-operator
   (fn [x]
     (binding [*mode* d/FORWARD-MODE]
       (g/partial-derivative x [])))
   g/derivative-symbol))

(def ^{:arglists '([f])}
  D-reverse
  "Reverse-mode derivative operator. Takes some function `f` and returns a
  function whose value at some point can multiply an increment in the arguments
  to produce the best linear estimate of the increment in the function value.

  For univariate functions, [[D-reverse]] computes a derivative. For vector-valued
  functions, [[D-reverse]] computes
  the [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
  of `f`."
  (o/make-operator
   (fn [x]
     (binding [*mode* d/REVERSE-MODE]
       (g/partial-derivative x [])))
   g/derivative-symbol))

(def ^{:arglists '([f])}
  D
  "Derivative operator. Takes some function `f` and returns a function whose value
  at some point can multiply an increment in the arguments to produce the best
  linear estimate of the increment in the function value.

  For univariate functions, [[D]] computes a derivative. For vector-valued
  functions, [[D]] computes
  the [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
  of `f`.

  The related [[emmy.env/Grad]] returns a function that produces a structure of
  the opposite orientation as [[D]]. Both of these functions use forward-mode
  automatic differentiation."
  D-forward)

(defn D-as-matrix [F]
  (fn [s]
    (matrix/s->m
     (s/compatible-shape (F s))
     ((D F) s)
     s)))

(defn partial-forward
  "Returns an operator that, when applied to a function `f`, produces a function
  that uses forward-mode automatic differentiation to compute the partial
  derivative of `f` at the (zero-based) slot index provided via `selectors`."
  [& selectors]
  (o/make-operator
   (fn [x]
     (binding [*mode* d/FORWARD-MODE]
       (g/partial-derivative x selectors)))
   `(~'partial ~@selectors)))

(defn partial-reverse
  "Returns an operator that, when applied to a function `f`, produces a function
  that uses reverse-mode automatic differentiation to compute the partial
  derivative of `f` at the (zero-based) slot index provided via `selectors`."
  [& selectors]
  (o/make-operator
   (fn [x]
     (binding [*mode* d/REVERSE-MODE]
       (g/partial-derivative x selectors)))
   `(~'partial ~@selectors)))

(def ^{:arglists '([& selectors])}
  partial
  "Returns an operator that, when applied to a function `f`, produces a function
  that uses forward-mode automatic differentiation to compute the partial
  derivative of `f` at the (zero-based) slot index provided via `selectors`."
  partial-forward)

;; ## Derivative Utilities
;;
;; Functions that make use of the differential operators defined above in
;; standard ways.

(defn taylor-series
  "Given a differentiable function `f` and any number of arguments `xs`, returns
  a [[emmy.series/PowerSeries]] representing the [Taylor
  series](https://en.wikipedia.org/wiki/Taylor_series) of the function `f`
  expanded at `xs`.

  Calling [[taylor-series]] with no arguments will return the [Maclaurin
  series](https://en.wikipedia.org/wiki/Taylor_series#List_of_Maclaurin_series_of_some_common_functions)
  of `f`, i.e., the Taylor series expansion at `(= x 0)`.

  Calling the returned power series with incremental argument `dx` will produce
  a [[emmy.series/Series]] representing the terms of the Taylor series of
  `f` expanded at `x` and evaluated at `x+dx`.

  NOTE: Just like the [[D]] operator, functions `f` of multiple-arguments are
  treated as a function of a single structural argument. If you pass multiple
  arguments `xs`, you'll have to manually wrap your multiple-argument `dx` in
  a [[emmy.structure/up]] or a vector before passing it to the returned
  power series.

  NOTE: The typical definition of a Taylor series of `f` expanded around some
  point `x` is

  $$T(p) = f(x) + \\frac{f'(x)}{1!}(p-x) + \\frac{f''(x)}{2!} (p-x)^2 + \\ldots,$$

  where `p` is the evaluation point. When `(= p x)`, all derivatives of the
  Taylor series expansion of `f` will exactly match the derivatives of `f`
  itself.

  The Taylor series returned here (call it $T'$) is actually a function of `dx`,
  where

  $$T'(dx) = T(x+dx) = f(x) + \\frac{f'(x)}{1!}(dx) + \\frac{f''(x)}{2!} (dx)^2 + \\ldots.$$"
  ([f] (taylor-series f 0))
  ([f & xs]
   (series/->function
    (apply ((g/exp D) f) xs))))

(defn symbolic-taylor-series
  "Similar to [[taylor-series]], except `f` is evaluated with symbolic arguments,
  and these arguments are only replaced with the values `xs` after Taylor series
  expansion.

  Please see the docs for [[taylor-series]]!"
  ([f] (symbolic-taylor-series f 0))
  ([f & xs]
   (let [syms      (map s/typical-object xs)
         replace-m (zipmap (flatten syms)
                           (flatten xs))
         series    (apply taylor-series f syms)]
     (letfn [(process-term [term]
               (g/simplify
                (s/mapr (fn rec [x]
                          (cond (d/dual? x)
                                (d/bundle-element
                                 (rec (d/primal x))
                                 (rec (d/tangent x))
                                 (d/tag x))

                                (tape/tape? x)
                                (tape/->TapeCell
                                 (tape/tape-tag x)
                                 (tape/tape-id x)
                                 (rec (tape/tape-primal x))
                                 (mapv (fn [[node partial]]
                                         [(rec node)
                                          (rec partial)])
                                       (tape/tape-partials x)))

                                :else (-> (g/simplify x)
                                          (x/substitute replace-m))))
                        term)))]
       (series/fmap process-term series)))))
