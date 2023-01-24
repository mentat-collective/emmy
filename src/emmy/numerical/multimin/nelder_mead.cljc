#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.multimin.nelder-mead
  (:require [emmy.util :as u]))

(defn- v+
  "add two vectors elementwise."
  [l r]
  (mapv + l r))

(defn- v-
  "subtract two vectors elementwise."
  [l r]
  (mapv - l r))

(defn- v*
  "multiply vector v by scalar s."
  [s v]
  (mapv #(* s %) v))

(defn ^:private initial-simplex
  "Takes an n-vector x0 and returns a list of n+1 n-vectors, of which x0 is the
  first, and the remainder are formed by perturbing each coordinate in turn."
  [x0 {:keys [nonzero-delta zero-delta]
       :or {nonzero-delta 0.05
            zero-delta 0.00025}}]
  (let [x0 (vec x0)
        scale (inc nonzero-delta)
        f (fn [i xi]
            (let [perturbed (if (zero? xi)
                              zero-delta
                              (* scale xi))]
              (assoc x0 i perturbed)))]
    (into [x0] (map-indexed f x0))))

(defn ^:private sup-norm
  "Returns the absolute value of the distance of the individual coordinate in any
  simplex farthest from its corresponding point in x0."
  [[x0 :as simplex]]
  (let [coords (if (sequential? x0)
                 (mapcat #(v- % x0) simplex)
                 (map #(- % x0) simplex))]
    (reduce max (map u/compute-abs coords))))

(defn ^:private counted
  "Takes a function and returns a pair of:
  - an atom that keeps track of fn invocation counts,
  - the instrumented fn"
  [f]
  (let [count (atom 0)]
    [count (fn [x]
             (swap! count inc)
             (f x))]))

(defn ^:private sort-by-f
  "Returns the two inputs `simplex` and `f(simplex)` sorted in ascending order by
  function value.
  Dimension must == the length of each element in the simplex."
  ([simplex f-simplex]
   (sort-by-f simplex f-simplex (count (peek simplex))))
  ([simplex f-simplex dimension]
   (let [indices-by-f (sort-by (partial nth f-simplex)
                               (range 0 (inc dimension)))
         sorted-simplex  (mapv simplex indices-by-f)
         sorted-fsimplex (mapv f-simplex indices-by-f)]
     [sorted-simplex sorted-fsimplex])))

(defn ^:private step-defaults
  "Generates the options required for a step of Nelder-Mead.

  `:adaptive?` controls the set of defaults. If true, they're generated using
  the supplied dimension; else, they're static."
  [dimension {:keys [adaptive?]
              :or {adaptive? true}
              :as m}]
  (let [base (if adaptive?
               {:alpha 1.0
                :beta (+ 1.0 (/ 2.0 dimension))
                :gamma (- 0.75 (/ (* 2.0 dimension)))
                :sigma (- 1.0 (/ dimension))}
               {:alpha 1.0
                :beta 2.0
                :gamma 0.5
                :sigma 0.5})]
    (merge base (select-keys m [:alpha :beta :gamma :sigma]))))

(defn ^:private step-fn
  "Returns a function that performs a single step of nelder-mead. The function
  expects a sorted simplex and f-simplex, and returns sorted results - a pair of
  - [simplex, f(simplex)]
  [This Scholarpedia
  page](http://www.scholarpedia.org/article/Nelder-Mead_algorithm) provides a
  nice overview of the algorithm.
  The parameters in opts follow the convention from [Gao and Han's
  paper](https://www.researchgate.net/publication/225691623_Implementing_the_Nelder-Mead_simplex_algorithm_with_adaptive_parameters)
  introducing the adaptive parameter version of Nelder-Mead:
  :alpha - reflection cefficient
  :beta  - expansion coefficient
  :gamma - contraction coefficient
  :sigma - shrink coefficient
  "
  ([f dimension opts]
   (let [{:keys [alpha beta sigma gamma]} (step-defaults dimension opts)]
     (letfn [(centroid-pt [simplex]
               (v* (/ dimension) (reduce v+ (pop simplex))))

             ;; Returns the point generated by reflecting the worst point across
             ;; the centroid of the simplex.
             (reflect [simplex centroid]
               (v- (v* (inc alpha) centroid)
                   (v* alpha (peek simplex))))

             ;; Returns the point generated by reflecting the worst point across
             ;; the centroid, and then stretching it in that direction by a factor
             ;; of beta.
             (reflect-expand [simplex centroid]
               (v- (v* (inc (* alpha beta)) centroid)
                   (v* (* alpha beta) (peek simplex))))

             ;; Returns the point generated by reflecting the worst point, then
             ;; shrinking it toward the centroid by a factor of gamma.
             (reflect-contract [simplex centroid]
               (v- (v* (inc (* gamma alpha)) centroid)
                   (v* (* gamma alpha) (peek simplex))))

             ;; Returns the point generated by shrinking the current worst point
             ;; toward the centroid by a factor of gamma.
             (contract [simplex centroid]
               (v+ (v* (- 1 gamma) centroid)
                   (v* gamma (peek simplex))))

             ;; Returns a simplex generated by scaling each point toward the best
             ;; point by the shrink factor $\sigma$; ie, by replacing all
             ;; points (except the best point $s_1$) with $s_i = s_1 + \sigma (\s_i
             ;; - s_1)$.
             (shrink [[s0 & rest]]
               (let [scale-toward-s0 #(v+ s0 (v* sigma (v- % s0)))
                     s (into [s0] (map scale-toward-s0 rest))]
                 (sort-by-f s (mapv f s) dimension)))]

       (fn [simplex [f-best :as f-simplex]]
         ;; Verify that inputs and outputs remain sorted by f value.
         {:pre [(apply <= f-simplex)]
          :post [#(apply <= (second %))]}
         (let [swap-worst (fn [elem f-elem]
                            (let [s  (conj (pop simplex) elem)
                                  fs (conj (pop f-simplex) f-elem)]
                              (sort-by-f s fs dimension)))
               f-worst    (peek f-simplex)
               f-butworst (peek (pop f-simplex))
               centroid   (centroid-pt simplex)
               reflected  (reflect simplex centroid)
               fr         (f reflected)]
           (cond
             ;; If the reflected point is the best (minimal) point so far, replace
             ;; the worst point with either an expansion of the simplex around that
             ;; point, or the reflected point itself.
             ;;
             ;; f(reflected worst) < f(best)
             (< fr f-best)
             (let [expanded (reflect-expand simplex centroid)
                   fe (f expanded)]
               (if (< fe fr)
                 (swap-worst expanded fe)
                 (swap-worst reflected fr)))

             ;; f(best) <= f(reflected worst) < f(second worst)
             ;;
             ;; Else, if the reflected worst point is better than the second worst
             ;; point, swap it for the worst point.
             (< fr f-butworst)
             (swap-worst reflected fr)

             ;; f(butworst) <= f(reflected worst) < f(worst)
             ;;
             ;; If the reflected point is still better than the worst point,
             ;; generated a point by shrinking the reflected point toward the
             ;; centroid. If this is better than (or equivalent to) the reflected
             ;; point, replace it. Else, shrink the whole simplex.
             (< fr f-worst)
             (let [r-contracted (reflect-contract simplex centroid)
                   frc (f r-contracted)]
               (if (<= frc fr)
                 (swap-worst r-contracted frc)
                 (shrink simplex)))

             ;; f(worst) <= f(reflected worst)
             ;;
             ;; Else, attempt to contrast the existing worst point toward the
             ;; centroid. If that improves performance, swap the new point; else,
             ;; shrink the whole simplex.
             :else
             (let [contracted (contract simplex centroid)
                   fc (f contracted)]
               (if (< fc f-worst)
                 (swap-worst contracted fc)
                 (shrink simplex))))))))))

(defn ^:private convergence-fn
  "Returns a function that returns true if the supplied simplex and simplex
  evaluations signal convergence, false otherwise."
  [{:keys [simplex-tolerance fn-tolerance]
    :or {simplex-tolerance 1e-4
         fn-tolerance 1e-4}}]
  (fn [simplex f-simplex]
    (and (<= (sup-norm simplex)   simplex-tolerance)
         (<= (sup-norm f-simplex) fn-tolerance))))

(defn ^:private stop-fn
  "Takes an atom that, when dereferenced, returns a function call count, and the
  dimension of the simplex.
  Returns a function of `iterations` that returns true if the iteration and
  function call limits signal stopping, false otherwise."
  [f-counter dimension {:keys [maxiter maxfun]}]
  (let [maxiter (or maxiter (* dimension 200))
        maxfun  (or maxfun (* dimension 200))]
    (fn [iterations]
      (or (> iterations maxiter)
          (> @f-counter maxfun)))))

(defn nelder-mead
  "Find the minimum of the function f: R^n -> R, given an initial point q ∈ R^n.
  Supports the following optional keyword arguments:

  `:callback` if supplied, the supplied fn will be invoked with iteration count,
  the values of X and the value of f(X) at each intermediate point of
  evaluation.

  `:info?` if true, wraps the result with evaluation information.

  `:adaptive?` if true, the Nelder-Mead parameters for contraction, expansion,
  reflection and shrinking will be set adaptively, as functions of the number of
  dimensions. If false they stay constant.

  `:alpha` sets the reflection coefficient used for each step of Nelder-Mead.

  `:beta` sets the expansion coefficient used for each step of Nelder-Mead.

  `:gamma` sets the contraction coefficient used for each step of Nelder-Mead.

  `:sigma` sets the shrink coefficient used for each step of Nelder-Mead.

  `:maxiter` Maximum number of iterations allowed for the minimizer. Defaults to
  200*dimension.

  `:maxfun` Maximum number of times the function can be evaluated before exiting.
  Defaults to 200*dimension.

  `:simplex-tolerance` When the absolute value of the max difference between the
  best point and any point in the simplex falls below this tolerance, the
  minimizer stops. Defaults to 1e-4.

  `:fn-tolerance` When the absolute value of the max difference between the best
  point's function value and the fn value of any point in the simplex falls
  below this tolerance, the minimizer stops. Defaults to 1e-4.

  `:zero-delta` controls the value to which 0 entries in the initial vector are
  set during initial simplex generation. Defaults to 0.00025.

  `:nonzero-delta` factor by which entries in the initial vector are perturbed to
  generate the initial simplex. Defaults to 0.05.

  See Gao, F. and Han, L.
      Implementing the Nelder-Mead simplex algorithm with adaptive
      parameters. 2012. Computational Optimization and Applications.
      51:1, pp. 259-277
  I gratefully acknowledge the [Python implementation in
  SciPy](https://github.com/scipy/scipy/blob/589c9afe41774ee96ec121f1867361146add8276/scipy/optimize/optimize.py#L556:5)
  which I have imitated here.
  "
  [func x0 {:keys [callback] :as opts}]
  (let [callback      (or callback (constantly nil))
        dimension     (count x0)
        [f-counter f] (counted func)
        step          (step-fn f dimension opts)
        convergence?  (convergence-fn opts)
        stop?         (stop-fn f-counter dimension opts)
        simplex       (initial-simplex x0 opts)
        f-simplex     (mapv f simplex)]
    (loop [[[s0 :as simplex] [f0 :as f-simplex]] (sort-by-f simplex f-simplex dimension)
           iteration 0]
      (callback iteration s0 f0)
      (let [converged? (convergence? simplex f-simplex)]
        (if (or converged? (stop? iteration))
          {:result s0
           :value f0
           :converged? converged?
           :iterations iteration
           :fncalls @f-counter}
          (recur (step simplex f-simplex)
                 (inc iteration)))))))