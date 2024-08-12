#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.unimin
  "`unimin` is a module of functions and methods designed to find minimal (or
  maximal) values of single variable functions.")

(defn local-maxima
  "Given a function f on [a, b] and N > 0, examine f at the endpoints a, b, and at
  N equally-separated interior points. From this form a list of brackets (p q)
  in each of which a local maximum is trapped. Then apply Brent to all these
  brackets and return a list of pairs (x fx) representing the local maxima.
  NOTE we switched to maximizer, opts...
  TODO consider conj, then rseq at the end if we really need them flipped."
  [f a b n maximizer opts]
  (let [h (/ (- b a) (+ n 1))
        xlist (mapv (fn [i] (if (= i (inc n))
                             b
                             (+ a (* i h))))
                    (range (+ n 2)))
        flist (mapv f xlist)
        xi (fn [i] (get xlist i))
        fi (fn [i] (get flist i))
        brack1 (if (> (fi 0) (fi 1))
                 [[(xi 0) (xi 1)]]
                 [])
        brack2 (if (> (fi (inc n)) (fi n))
                 (cons [(xi n) (xi (+ n 1))] brack1)
                 brack1)
        bracketlist
        (loop [i 1
               b brack2]
          (if (> i n)
            b
            (if (and (<= (fi (dec i)) (fi i))
                     (>= (fi i) (fi (inc i))))
              (recur (inc i) (cons [(xi (- i 1))
                                    (xi (+ i 1))]
                                   b))
              (recur (inc i) b))))]
    (map (fn [[a b]]
           (maximizer f a b opts))
         bracketlist)))

;; TODO this API makes no sense anymore...
(defn local-minima [f a b n maximizer opts]
  (let [g (fn [x] (- (f x)))
        result (local-maxima g a b n maximizer opts)]
    ;; TODO this can't be right, make it work for the actual return type.
    (map (fn flip [[a b c]]
           [a (- b) c])
         result)))

;; TODO these too are basically the same, consolidate!

(defn estimate-global-max
  "Refer to the previous two functions and find the max of all of those."
  [f a b n maximizer opts]
  (let [local-maxs (local-maxima f a b n maximizer opts)]
    (loop [best-so-far (first local-maxs)
           unexamined (rest local-maxs)]
      (if (empty? unexamined)
        best-so-far
        (let [next (first unexamined)]
          (if (> (second next)
                 (second best-so-far))
            (recur next (rest unexamined))
            (recur best-so-far (rest unexamined))))))))

(defn estimate-global-min
  "Refer to the previous two functions and find the min."
  [f a b n maximizer ftol]
  (let [local-mins (local-minima f a b n maximizer ftol)]
    (loop [best-so-far (first local-mins)
           unexamined (rest local-mins)]
      (if (empty? unexamined)
        best-so-far
        (let [next (first unexamined)]
          (if (< (second next)
                 (second best-so-far))
            (recur next (rest unexamined))
            (recur best-so-far (rest unexamined))))))))
