#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.ode
  "ODE solvers for working with initial value problems."
  (:require #?(:cljs ["odex" :as o])
            #?(:clj [clojure.core.async :as a])
            [clojure.core.reducers :as r]
            [emmy.expression.compile :as c]
            [emmy.structure :as struct]
            #?(:clj [emmy.util :as u])
            [emmy.util.stopwatch :as us]
            [emmy.value :as v]
            [taoensso.timbre :as log])
  #?(:clj
     (:import (org.apache.commons.math3.ode.nonstiff GraggBulirschStoerIntegrator)
              (org.apache.commons.math3.ode FirstOrderDifferentialEquations)
              (org.apache.commons.math3.ode ExpandableStatefulODE)
              (org.apache.commons.math3.ode.sampling StepHandler))))

(def ^:private near? (v/within 1e-8))
(def ^:private default-epsilon 1e-8)

(defn- flatten-into-primitive-array
  "Copy the sequence `xs` into the primitive double array `arr`."
  [^doubles arr xs]
  (let [ix (atom -1)]
    (r/reduce (fn [^doubles a ^double x]
                (aset a (swap! ix unchecked-inc) x)
                a)
              arr
              (r/flatten xs))))

(defn stream-integrator
  "Produces a function, monotonic in its single numeric argument,
   that represents the integral of the function f' given the initial
   data $y_0 = f(x_0)$ and an options dictionary (presently containing
   the tolerance for error $\\epsilon$, but eventually also selecting
   from a menu of integration techniques).

   This is done by creating an adaptive step-size ODE solver, and
   advancing its steps as needed to supply function values. (This
   architecture accounts for why the arguments to f must be presented
   in order). Old solution segments are discarded. The goal of this
   approach is to avoid the requirement of supplying an upper limit
   to the integration. At the cost of requiring monotonic arguments
   to f, the integrated function can essentially be used forever
   without accumulating unbounded state.

   The function `f'` should have the signature `[x y y']`, where `y'` is a
   primitive double array, which the function should fill in based
   on the values `x` and `y`.) Both `y` and `y'` will be primitive arrays
   of type double, the same length as that of `y0`. Both arrays are
   owned by the integrator. In particular, y should never be modified,
   and neither array should be modified or expected to persist after
   the return of `f'`. This approach has observable memory and
   performance impacts.

   The return value of the integrating function, however, is newly
   allocated and belongs to the caller.

   The integrating function may be called with no arguments to shut down
   the integration, allowing for the final reclamation of its resources.

   When the ODE solver is provided by Java, it may be necessary to
   use an auxiliary thread to enable this style of flow control.  If
   JavaScript, we expect the solver to provide a generator of solution
   segments."
  [f' x0 y0 {:keys [epsilon] :or {epsilon default-epsilon}}]
  (let [dimension (count y0)]
    #?(:clj
       (let [gbs               (GraggBulirschStoerIntegrator. 0. 1. (double epsilon) (double epsilon))
             ode               (ExpandableStatefulODE.
                                (reify FirstOrderDifferentialEquations
                                  (computeDerivatives [_ x y out] (f' x y out))
                                  (getDimension [_] dimension)))
             step-requests     (a/chan)
             solution-segments (a/chan)]
         (doto ode
           (.setTime x0)
           (.setPrimaryState (double-array y0)))
         (.addStepHandler
          gbs
          (reify StepHandler
            (init [_ _ _ _])
            (handleStep
             [_ interpolator _]
              ;; The `step-requests` channel sends `true` each time a new segment of
              ;; the solution is demanded; `false` is a signal that the consumer
              ;; has no further need of them. When sending segments back, receiving
              ;; false from the send operation indicates that the channel is closed.
              ;; In either of these cases the integration should be shut down.
              ;;
              ;; Normally, the use of an exception for flow control is frowned upon.
              ;; Ideally, the CM3 integrator would look at the return value of the
              ;; step handler to decide whether or not to continue, but it doesn't.
              ;; (You can install an EventHandler, which watches an indicator function
              ;; whose change of sign can halt the integration, but using that feature
              ;; for this purpose would be contrived, and the extra work the integrator
              ;; must do to watch the indicator function is not justified by this
              ;; goal.) Handling one exception in a worker thread is a small cost.
              ;;
              ;; The function returned in the solution segment object is evaluated
              ;; using state that the integrator maintains, and which will be
              ;; overwritten when the next state is computed. Use of the function
              ;; after the next step-request is made will result in undefined
              ;; behavior.
             (when-not (and (a/>!! solution-segments
                                   (let [x0 (.getPreviousTime interpolator)
                                         x1 (.getCurrentTime interpolator)]
                                     {:x0 x0
                                      :x1 x1
                                      :f  (fn [x]
                                            (assert (and (>= x x0)
                                                         (<= x x1))
                                                    (format "ODE interpolation request %g out of range [%g, %g]"
                                                            x x0 x1))
                                            (.setInterpolatedTime interpolator x)
                                            (.getInterpolatedState interpolator))}))
                            (a/<!! step-requests))
               (u/interrupted "end of integration")))))
         (doto (Thread.
                (fn []
                  ;; Wait for the first step request before calling integrate.
                  ;; Our flow control for the CM3 integrator is through the
                  ;; StepHandler callback; when `.integrate` is called, the first
                  ;; step will be computed, and we want to hold the callback from
                  ;; returning until the client signals that they are done with the
                  ;; (very stateful!) interpolation function, in order to prevent
                  ;; the generation of another step's worth of interpolation data
                  ;; before we are ready. Therefore we enforce the invariant that
                  ;; a step request will precede the computation of any step, which
                  ;; is why we pull from the channel here.
                  (when-not (a/<!! step-requests)
                    (u/interrupted "end of integration"))
                  (try
                    (.integrate gbs ode Double/MAX_VALUE)
                    ;; InterruptedException is the nominal way to exit an integration
                    (catch InterruptedException _)
                    (catch Throwable t
                      ;; If the integrator throws an exception, send that exception through
                      ;; the `solution-segments` channel so that it may be handled by the consumer
                      ;; thread.
                      (a/>!! solution-segments t)))))
           (.setDaemon true)
           (.start))
         (let [next-segment (fn []
                              (a/>!! step-requests true)
                              (let [v (a/<!! solution-segments)]
                                (when (u/throwable? v)
                                  (throw v))
                                v))
               current-segment (atom (next-segment))]
           (fn
             ([]
              (a/>!! step-requests false))
             ([x]
              (when (< x (:x0 @current-segment))
                (u/illegal-state "Cannot use interpolation function in backwards direction"))
              (while (> x (:x1 @current-segment))
                (let [s (next-segment)]
                  (reset! current-segment s)))
              (into [] ((:f @current-segment) x))))))

       :cljs
       (let [solver (o/Solver.
                     f'
                     dimension
                     #js {:absoluteTolerance epsilon
                          :relativeTolerance epsilon
                          :rawFunction true})]
         (comp js->clj (.integrate solver x0 (double-array y0)))))))

(defn ^:no-doc integration-opts
  "Returns a map with the following kv pairs:

  - `:integrator` an instance of `GraggBulirschStoerIntegrator`
  - `:stopwatch` [[IStopwatch]] instance that records total evaluation time inside
    the derivative function
  - `:counter` an atom containing a `Long` that increments every time derivative fn
    is called."
  [state-derivative derivative-args initial-state
   {:keys [compile?] :as opts}]
  (let [evaluation-time    (us/stopwatch :started? false)
        evaluation-count   (atom 0)
        flat-initial-state (flatten initial-state)
        primitive-params   (double-array derivative-args)
        derivative-fn      (if compile?
                             (c/compile-state-fn state-derivative derivative-args initial-state
                                                 {:calling-convention :primitive
                                                  :generic-params? false})
                             (do (log/warn "Not compiling function for ODE analysis")
                                 (let [d:dt (apply state-derivative derivative-args)]
                                   (fn [ys yps _]
                                     (flatten-into-primitive-array yps (d:dt (struct/unflatten ys initial-state))))
                                   )))
        equations        (fn [_ y out]
                           ;; TODO: should we consider allowing an option to add a dummy
                           ;; x-parameter in the compiled code, which would allow unwrapping
                           ;; this last layer?
                           (us/start evaluation-time)
                           (derivative-fn y out primitive-params)
                           (us/stop evaluation-time)
                           (swap! evaluation-count inc))

        integrator (stream-integrator equations 0 flat-initial-state opts)]
    {:integrator integrator
     :stopwatch evaluation-time
     :counter evaluation-count}))

(defn make-integrator
  "make-integrator takes a state derivative function (which in this
  system is assumed to be a map from a structure to a structure of the
  same shape, as differentiating a function does not change its
  shape), and returns an integrator, which is a function of several
  arguments:

  - the initial state
  - an intermediate-state observation function
  - the step size desired
  - the final time to seek, and
  - an error tolerance.

  If the `observe` function is not nil, it will be invoked with the time as
  first argument and integrated state as the second, at each intermediate step."
  [state-derivative derivative-args]
  (fn call
    ([initial-state step-size t]
     (call initial-state step-size t {}))
    ([initial-state step-size t {:keys [observe] :as opts}]
     (let [total-time (us/stopwatch :started? true)
           latest     (atom [0 nil])
           {:keys [integrator stopwatch counter]}
           (integration-opts state-derivative derivative-args initial-state opts)
           array->state #(struct/unflatten % initial-state)
           step (fn [x]
                  (let [y (array->state (integrator x))]
                    (when observe (observe x y))
                    (reset! latest [x y])))]
       (when observe
         (doseq [x (range 0 t step-size)]
           (step x)))
       (when (not (near? t (nth @latest 0)))
         (step t))
       (integrator)
       (us/stop total-time)
       (log/info "#" @counter "total" (us/repr total-time) "f" (us/repr stopwatch))
       (nth @latest 1)))))

(defn state-advancer
  "state-advancer takes a state derivative function constructor followed by the
  arguments to construct it with. The state derivative function is constructed
  and an integrator is produced which takes:

  - initial state
  - target time

  as arguments. Optionally, supply an options map with these optional fields:

  `:compile?`: If true, the ODE solver will compile your state function.

  `:epsilon`: The maximum error tolerance allowed by the ODE solver, both
  relative and absolute.

  Returns the final state.

  The state derivative is expected to map a structure to a structure of the same
  shape, and is required to have the time parameter as the first element."
  [state-derivative & state-derivative-args]
  (let [I (make-integrator state-derivative state-derivative-args)]
    (fn call
      ([initial-state t]
       (call initial-state t {}))
      ([initial-state t opts]
       (I initial-state 0 t opts)))))

(defn evolve
  "evolve takes a state derivative function constructor and its arguments, and
  returns an integrator via make-integrator.

  In particular, the returned function accepts a callback function which will be
  invoked at intermediate grid points of the integration."
  [state-derivative & state-derivative-args]
  (make-integrator state-derivative state-derivative-args))

(defn integrate-state-derivative
  "A wrapper for evolve, which is more convenient when you just
  want a vector of (time, state) pairs over the integration interval
  instead of having to deal with a callback. Integrates the supplied
  state derivative (and its argument package) from [0 to t1] in steps
  of size dt"
  [state-derivative state-derivative-args initial-state t1 dt]
  (let [opts (integration-opts state-derivative
                               state-derivative-args
                               initial-state
                               {})
        f (:integrator opts)]
    (try
      (mapv f (for [x (range 0 (+ t1 dt) dt)
                    :when (< x (+ t1 (/ dt 2)))]
                x))
      (finally (f)))))
