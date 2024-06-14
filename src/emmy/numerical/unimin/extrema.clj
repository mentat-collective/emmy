#_"SPDX-License-Identifier: GPL-3.0"

^#:nextjournal.clerk
        {:toc true
         :visibility :hide-ns}
(ns emmy.numerical.unimin.extrema
  (:require [emmy.generic :as g]
            [emmy.numerical.unimin.golden :as ug]
            [emmy.util :as u]
            [emmy.value :as v]))

;; # Find Local Maxima
;; Given a function f on [a, b] and N > 0, examine f at the endpoints
;; xa, xb, and at N equally-separated interior points. From this form a
;; list of brackets (p q) in each of which a local maximum is trapped.
;; Then apply Golden Section to all these brackets and return a list of
;; pairs (x fx) representing the local maxima.

(defn local-maxima [f xa xb n ftol]
  "Find and bracket n+1 local maxima in the given interval"
  (let [h (/ (- xb xa) (inc n))
        xs (map #(if (= % (inc n)) xb (+ xa (* % h))) (range (+ n 2)))
        fs (map f xs)
        xi #(nth xs %)
        fi #(nth fs %)
        bracket1 (if (> (fi 0) (fi 1))
                   (list (list (xi 0) (xi 1)))
                   nil)
        bracket2 (if (> (fi (inc n)) (fi n))
                   (cons (list (xi n) (xi (inc n))) bracket1)
                   bracket1)
        bracket-list (loop [i 1
                            b bracket2]
                       (if (> i n)
                         b
                         (if (and (<= (fi (dec i)) (fi i))
                                  (>= (fi i) (fi (inc i))))
                           (recur (inc i) (cons (list (xi (dec i))
                                                      (xi (inc i)))
                                                b))
                           (recur (inc i) b))))
        locmax (fn [int]
                 (ug/golden-section-max f (first int) (second int)
                                        {:fn-tolerance ftol}))]
    (map locmax bracket-list)
    ))

(defn local-minima [f xa xb n ftol]
  "Find and bracket n+1 local minima in the given interval"
  (let [g #(- (f %))
        result (local-maxima g xa xb n ftol)   ; local-maxima returns a list of maps
        ; we need to negate the second key (:value)
        flip (fn [result-map] (update result-map :value #(- %))) ]
    (map flip result)))

(defn estimate-global-max [f xa xb n ftol]
  "Get the global maximum estimate from bracketed maxima"
  (let [local-maxs (local-maxima f xa xb n ftol)]
    (loop [best-so-far (first local-maxs)
           unexamined (rest local-maxs)]
      (if (empty? unexamined)
        best-so-far
        (let [next (first unexamined)]
          (if (> (next :value) (best-so-far :value))
            (recur next (rest unexamined))
            (recur best-so-far (rest unexamined))))))))

(defn estimate-global-min [f xa xb n ftol]
  "Get the global minimum estimate from bracketed minima"
  (let [local-mins (local-minima f xa xb n ftol)]
    (loop [best-so-far (first local-mins)
           unexamined (rest local-mins)]
      (if (empty? unexamined)
        best-so-far
        (let [next (first unexamined)]
          (if (< (next :value) (best-so-far :value))
            (recur next (rest unexamined))
            (recur best-so-far (rest unexamined))))))))

