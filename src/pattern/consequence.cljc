#_"SPDX-License-Identifier: GPL-3.0"

(ns pattern.consequence
  "Code for defining and compiling 'consequence' functions, ie, functions from a
  binding map generated by a [[[pattern.match]]] to some successful
  transformation (or a failure!).

  See [[pattern.rule]] for a detailed treatment."
  (:require [pattern.syntax :as ps]
            [sicmutils.util :as u]))

;; ## Consequence Functions
;;
;; A consequence is a function from a binding dictionary (produced by a matcher)
;; to a successful result or a failure. A "rule" (see `pattern.rule`) is built
;; out of a matcher (from `pattern.match`) and a consequence function.
;;
;; The contract for a "consequence" function is that it can return `false`,
;; `nil` or [[pattern.match/failure]] to signal failure. But what if the
;; function wants to succeed with `false` or `nil`?
;;
;; Wrapping a return value with [[succeed]] will allow a successful return of
;; `false` or `nil`; a rule using a consequence function uses [[unwrap]] to
;; retrieve the value before returning it.

(defn succeed
  "Wraps the argument `x` in a form that will always successfully return from a
  consequence function, whatever its value.

  Use [[succeed]] to return `nil` or `false` from a consequence function. For
  all other return values, returning `(succeed x)` is identical to returning
  `x`"
  [x]
  {::succeed x})

(defn unwrap
  "Given a form returned by a consequence function, unwraps the top level
  `succeed` wrapper if present to return the final value."
  [x]
  (if (map? x)
    (::succeed x x)
    x))

;; ### Consequence Skeletons
;;
;; A Skeleton is a template of the form that we'd like to return from a
;; consequence function, with pattern-matching variables like `?x` and `??x` in
;; place of binding map lookups.
;;
;; [[compile-skeleton]] transforms an expression like

(comment
  (let [cake 10]
    (+ ?x ?y ~cake (? (fn [m] (m '?z))))))

;; Into a form like the following, meant to be evaluated in an environment where
;; `m` is bound to some map of bindings (the user provides this symbol):

(comment
  (let [cake 10]
    (list '+ (m '?x) (m '?y) cake ((fn [m] (m '?z)) m))))

;; See [[compile-skeleton]] for the full set of transformation rules.

(defn- apply-form
  "Given symbols `f` representing a function and `x` representing its argument,
  returns a form that represents function application.

  - Symbols are quoted
  - [[unquote?]] forms are included without quote
  - all other forms are left untouched."
  [f x]
  (let [f (cond (simple-symbol? f) `(quote ~f)
                (ps/unquote? f) (ps/unquoted-form f)
                :else f)]
    (list f x)))

(defn compile-skeleton
  "Takes:

  - a symbol `frame-sym` meant to reference a map of bindings
  - a skeleton expression `skel`

  and returns an unevaluated body that, when evaluated, will produce a form
  structure of identical shape to `skel`, with:

  - all variable binding forms replaced by forms that look up the binding in a
    map bound to `frame-sym`
  - same with any segment binding form, with the added note that these should
    be spliced in
  - any `unquote` or `unquote-splicing` forms respected."
  [frame-sym skel]
  (letfn [(compile-sequential [xs]
            (let [acc (ps/splice-reduce (some-fn ps/segment?
                                                 ps/reverse-segment?
                                                 ps/unquote-splice?)
                                        compile xs)]
              (cond (empty? acc) ()
                    (= 1 (count acc)) (first acc)
                    :else `(concat ~@acc))))

          (compile [form]
            (cond (or (ps/binding? form)
                      (ps/segment? form))
                  (let [v (ps/variable-name form)]
                    (apply-form v frame-sym))

                  (ps/reverse-segment? form)
                  (let [v (ps/reverse-segment-name form)]
                    (list `rseq (apply-form v frame-sym)))

                  (symbol? form) (list 'quote form)

                  (ps/unquote? form)
                  (ps/unquoted-form form)

                  (ps/unquote-splice? form)
                  (into [] (ps/unquoted-form form))

                  (map? form)
                  (u/map-vals compile form)

                  (vector? form)
                  `(vec ~(compile-sequential form))

                  (sequential? form)
                  (if (empty? form)
                    form
                    `(seq ~(compile-sequential form)))

                  :else form))]
    (if skel
      `(let [r# ~(compile skel)]
         (or r# (succeed r#)))
      `(succeed ~skel))))
