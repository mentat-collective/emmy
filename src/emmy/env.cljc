#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.env
  "Batteries-included namespace for the [Emmy](https://github.com/mentat-collective/emmy/) library.

  The purpose of [[emmy.env]] is to bundle all of the functions used
  in [Structure and Interpretation of Classical
  Mechanics](https://tgvaughan.github.io/sicm/) and [Functional Differential
  Geometry](https://mitpress.mit.edu/books/functional-differential-geometry)
  into a single scope. The following form will import everything
  from [[emmy.env]] into your REPL:

  ```clojure
  (require '[emmy.env :as e])
  (e/bootstrap-repl!)
  ```

  Or, in Clojure:

  ```clojure
  (require '[emmy.env :as e :refer :all])
  ```"
  (:refer-clojure
   :exclude [+ - * / zero? compare divide
             numerator denominator
             infinite? abs
             ref partial =])
  (:require [clojure.core :as core]
            [emmy.abstract.function :as af]
            [emmy.abstract.number]
            [emmy.algebra.fold]
            [emmy.calculus.basis]
            [emmy.calculus.connection]
            [emmy.calculus.coordinate :as cc]
            [emmy.calculus.covariant]
            [emmy.calculus.curvature]
            [emmy.calculus.derivative :as d]
            [emmy.calculus.form-field]
            [emmy.calculus.frame :as cf]
            [emmy.calculus.hodge-star]
            [emmy.calculus.indexed :as ci]
            [emmy.calculus.manifold]
            [emmy.calculus.map]
            [emmy.calculus.metric :as cm]
            [emmy.calculus.vector-calculus]
            [emmy.calculus.vector-field]
            [emmy.complex]
            [emmy.dual]
            [emmy.expression]
            [emmy.expression.render :as render]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.matrix :as matrix]
            [emmy.mechanics.hamilton]
            [emmy.mechanics.lagrange]
            [emmy.mechanics.noether]
            [emmy.mechanics.rigid]
            [emmy.mechanics.rotation]
            [emmy.mechanics.routhian]
            [emmy.mechanics.time-evolution]
            [emmy.modint]
            [emmy.numerical.derivative]
            [emmy.numerical.minimize]
            [emmy.numerical.multimin.nelder-mead]
            [emmy.numerical.ode]
            [emmy.numerical.quadrature]
            [emmy.numerical.unimin.brent]
            [emmy.numerical.unimin.golden]
            [emmy.operator :as o]
            [emmy.polynomial.factor]
            [emmy.quaternion]
            [emmy.ratio]
            [emmy.series :as series]
            [emmy.simplify]
            [emmy.special.elliptic]
            [emmy.special.factorial]
            [emmy.sr.boost]
            [emmy.sr.frames]
            [emmy.structure :as structure]
            [emmy.util :as u]
            [emmy.util.aggregate]
            [emmy.util.def :refer [import-def import-vars]]
            [emmy.util.permute]
            [emmy.util.stream :as us]
            [emmy.value :as v])
  #?(:cljs
     (:require-macros [emmy.env])))

(defmacro bootstrap-repl!
  "Bootstraps a repl or Clojure namespace by requiring all public vars
  from [[emmy.env]].

  (This will only work at a repl in ClojureScript.)

  TODO add `:rename`, `:exclude` support."
  []
  `(require '~['emmy.env
               :refer
               (into [] (keys (ns-publics 'emmy.env)))]))

(u/sci-macro literal-function
  ([f] `(af/literal-function ~f))
  ([f sicm-signature]
   (if (and (list? sicm-signature)
            (core/= '-> (first sicm-signature)))
     `(af/literal-function ~f '~sicm-signature)
     `(af/literal-function ~f ~sicm-signature)))
  ([f domain range]
   `(af/literal-function ~f ~domain ~range)))

(u/sci-macro with-literal-functions [& args]
  `(af/with-literal-functions ~@args))

(u/sci-macro let-coordinates
  "similar to a `let` binding that holds pairs of

  <coordinate-structure-prototype>, <coordinate-system>

  And internally binds, for each pair: (take `[x y]` and `m/R2-rect` as
  examples):

  - The coordinate system symbol `R2-rect` to a new version of the coordinate
    system with its `coordinate-prototype` replaced by the one you supplied.
    That's `(up x y)` in this example.

  - the entries `x` and `y` to coordinate functions, i.e., functions from manifold
    point to this particular coordinate

  - `d:dx` and `d:dy` vector field procedures (I'm fuzzy here!)

  - `dx` and `dy` 1-forms for each coordinate (fuzzy here too!)

  Example:

  ```clojure
  (let-coordinates [[x y]    R2-rect
                   [r theta] R2-polar]
    ;; bindings:
    ;; R2-rect, x, y, d:dx, d:dy, dx, dy
    ;; R2-polar, r, theta, d:dr, d:dtheta, dr, dtheta
    body...)
  ```"
  [bindings & body]
  `(cc/let-coordinates ~bindings ~@body))

(u/sci-macro using-coordinates
  "[[using-coordinates]] wraps [[let-coordinates]] and allows you to supply a
  single coordinate prototype and a single coordinate system.
  See [[let-coordinates]] for details about what symbols are bound inside the
  body.

  Example:

  ```clojure
  (using-coordinates (up x y) R2-rect
                     body...)
  ```"
  [coordinate-prototype coordinate-system & body]
  `(cc/using-coordinates ~coordinate-prototype
                         ~coordinate-system
                         ~@body))

(u/sci-macro define-coordinates
  "Given some `coordinate-system` like `R2-rect` and a `coordinate-prototype` like
  `[x y]` or `(up x y), `binds the following definitions into the namespace
  where [[define-coordinates]] is invoked:

  - `R2-rect` binds to a new version of the coordinate system with its
    `coordinate-prototype` replaced by the supplied prototype

  - `x` and `y` bind to coordinate functions, i.e., functions from manifold point
  to that particular coordinate

  - `d:dx` and `d:dy` bind to the corresponding vector field procedures

  - `dx` and `dy` bind to 1-forms for each coordinate."
  [coordinate-prototype coordinate-system]
  `(cc/define-coordinates ~coordinate-prototype ~coordinate-system))

(defn ref
  "A shim so that ref can act like nth in SICM contexts, as clojure core ref
  elsewhere."
  ([a] #?(:clj (core/ref a) :cljs a))
  ([a & ks]
   (cond (f/function? a) (f/compose #(apply ref % ks) a)
         (o/operator? a) (o/make-operator
                          (f/compose #(apply ref % ks) (o/procedure a))
                          `(~'compose (~'component ~@ks)
                            ~(o/name a)))
         :else (if (and (associative? a)
                        (every? v/integral? ks))
                 (if (matrix/matrix? a)
                   (matrix/get-in a ks)
                   (get-in a ks))
                 #?(:clj (apply core/ref a ks)
                    :cljs (get-in a ks))))))

(defn component
  "Given a sequence of `selectors`, return a function that accepts some object `x`
  and returns:

  ```clojure
  (apply ref x selectors)
  ```
  "
  [& selectors]
  (fn [x] (apply ref x selectors)))

(defn partial
  "A shim. Dispatches to [[d/partial]] when all the arguments are integers; falls
  back to [[clojure.core/partial]] (partial function application) otherwise."
  [& selectors]
  (if (every? integer? selectors)
    (apply d/partial selectors)
    (apply core/partial selectors)))

;; Constants

(def ^:const pi
  "The mathematical constant [Pi](https://en.wikipedia.org/wiki/Pi)."
  Math/PI)

(def ^:const -pi
  "The negation of the mathematical
  constant [Pi](https://en.wikipedia.org/wiki/Pi)."
  (g/- Math/PI))

(def ^:const tau
  "The mathematical
  constant [Tau](https://en.wikipedia.org/wiki/Turn_(angle)#Tau_proposals),
  equal to $2\\pi$."
  (g/* 2 Math/PI))

(def ^:const -tau
  "The negation of the mathematical
  constant [Tau](https://en.wikipedia.org/wiki/Turn_(angle)#Tau_proposals),
  equal to $-2\\pi$."
  (g/- tau))

(def ^:const euler
  "The mathematical
  constant [e](https://en.wikipedia.org/wiki/E_(mathematical_constant)),
  sometimes known as Euler's Number."
  Math/E)

(def ^:const euler-gamma
  "The mathematical constant known as the [Euler–Mascheroni
  constant](https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant) and
  sometimes as Euler's constant."
  0.57721566490153286)

(def ^:const phi
  "The mathematical constant [𝜑](https://en.wikipedia.org/wiki/Golden_ratio), also
  known as the Golden Ratio."
  (g/divide
   (inc (Math/sqrt 5.0)) 2.0))

(import-def structure/generate s:generate)
(import-def matrix/generate m:generate)
(import-def structure/basis-unit v:make-basis-unit)

(import-def matrix/by-rows matrix-by-rows)
(import-def matrix/by-cols matrix-by-cols)
(import-def matrix/row row-matrix)
(import-def matrix/column column-matrix)

(import-def series/constant constant-series)
(import-def series/sum series:sum)
(import-def cm/invert metric:invert)

(import-def us/seq-print seq:print)
(import-def us/pprint seq:pprint)

(import-def ci/outer-product i:outer-product)
(import-def ci/contract i:contract)

(import-def cf/params frame-params)

(defn tex$
  "Returns a string containing a LaTeX representation of `expr`, wrapped in single
  `$` to mark the string as an inline LaTeX form."
  [expr]
  (str "$" (-> expr g/simplify render/->TeX) "$"))

(defn tex$$
  "Returns a string containing a LaTeX representation of `expr`, wrapped in double
  `$$` to mark the string as a block LaTeX form."
  [expr]
  (str "$$" (-> expr g/simplify render/->TeX) "$$"))

(defn ->tex-equation
  "Returns a string containing a LaTeX representation of `expr`, wrapped in an
  `equation` environment.

  Optionally supply a `:label` keyword argument to set a custom label."
  [expr & {:keys [label]}]
  (-> (g/simplify expr)
      (render/->TeX :equation (or label true))))

(import-vars
 [emmy.abstract.number literal-number]
 [emmy.complex complex complex?]
 [emmy.function arity compose arg-shift arg-scale I]
 [emmy.modint chinese-remainder]
 [emmy.operator commutator anticommutator]
 [emmy.polynomial.factor factor]
 [emmy.ratio numerator denominator #?@(:cljs [ratio? rationalize])]
 [emmy.series series power-series binomial-series partial-sums]
 [emmy.util bigint? #?@(:cljs [bigint])]

 [emmy.generic
  * + - / divide
  negate
  negative? infinite?
  invert
  abs
  sqrt
  quotient remainder modulo
  floor ceiling
  integer-part fractional-part
  expt
  exp exp2 exp10
  log log2 log10
  gcd lcm
  exact-divide
  square cube
  cos sin tan
  cot sec csc
  atan
  acos asin acot asec acsc
  cosh sinh tanh coth sech csch
  acosh asinh atanh acoth asech acsch
  sinc tanc sinhc tanhc
  make-rectangular make-polar
  real-part imag-part
  magnitude angle conjugate
  transpose trace determinant dimension
  dot-product inner-product outer-product cross-product
  partial-derivative Lie-derivative
  solve-linear solve-linear-left solve-linear-right
  simplify
  zero? one? identity?
  zero-like one-like identity-like
  exact? freeze]
 [emmy.structure
  compatible-shape
  compatible-zero dual-zero
  down
  mapr sumr
  orientation
  structure->vector
  structure?
  up
  up?
  vector->down vector->up
  literal-down literal-up]
 [emmy.expression
  expression-of
  expression->stream expression->string print-expression pe]
 [emmy.expression.render
  ->infix
  ->TeX
  ->JavaScript]

 ;; Calculus Namespaces

 [emmy.calculus.basis
  basis? coordinate-basis? make-basis
  coordinate-system->basis
  basis->coordinate-system
  basis->oneform-basis
  basis->vector-basis
  basis->dimension
  contract
  vector-basis->dual
  make-constant-vector-field
  Jacobian]

 [emmy.calculus.coordinate coordinate-functions]

 [emmy.calculus.connection
  make-Christoffel-1
  metric->Christoffel-1 metric->Christoffel-2
  literal-Christoffel-1 literal-Christoffel-2
  metric->connection-1 metric->connection-2
  literal-Cartan
  structure-constant]

 [emmy.calculus.covariant
  covariant-derivative
  covariant-differential
  Lie-D
  interior-product
  make-Cartan Cartan? Cartan->forms Cartan->basis
  make-Christoffel Christoffel? Christoffel->symbols Christoffel->basis
  Cartan->Christoffel
  Christoffel->Cartan
  symmetrize-Christoffel
  symmetrize-Cartan
  Cartan-transform
  Cartan->Cartan-over-map
  geodesic-equation parallel-transport-equation]

 [emmy.calculus.curvature
  Riemann-curvature Riemann Ricci torsion-vector torsion
  curvature-components]

 [emmy.calculus.derivative
  D D-forward D-reverse
  partial-forward partial-reverse
  D-as-matrix taylor-series]

 [emmy.calculus.form-field
  form-field? nform-field? oneform-field?
  ff:zero
  components->oneform-field
  oneform-field->components
  literal-oneform-field
  coordinate-basis-oneform-field
  coordinate-system->oneform-basis
  basis-components->oneform-field
  oneform-field->basis-components
  function->oneform-field
  wedge
  Alt alt-wedge
  exterior-derivative d]

 [emmy.calculus.frame
  frame? make-event event? claim
  coords->event event->coords ancestor-frame frame-name
  frame-owner frame-maker]

 [emmy.calculus.hodge-star
  Gram-Schmidt orthonormalize
  Hodge-star]

 [emmy.calculus.indexed
  argument-types with-argument-types
  index-types with-index-types
  typed->indexed indexed->typed
  typed->structure structure->typed]

 [emmy.calculus.manifold
  make-manifold coordinate-system-at
  manifold-type
  patch-names coordinate-system-names
  manifold?
  manifold-family?
  manifold-point?
  chart point
  typical-coords typical-point transfer-point
  corresponding-velocities
  literal-manifold-function
  zero-manifold-function one-manifold-function
  constant-manifold-function
  coordinate-system?
  Rn
  R1 R1-rect the-real-line
  R2 R2-rect R2-polar
  R3 R3-rect R3-cyl R3-spherical
  R4 R4-rect R4-cyl
  spacetime spacetime-rect spacetime-sphere
  Sn
  S1 S1-circular S1-tilted S1-slope S1-gnomonic
  S2-type S2 S2-spherical S2-tilted S2-stereographic S2-Riemann S2-gnomonic
  S2p S2p-spherical S2p-tilted S2p-stereographic S2p-Riemann S2p-gnomonic
  S3 S3-spherical S3-tilted S3-stereographic S3-gnomonic
  SO3-type SO3 Euler-angles alternate-angles]

 [emmy.calculus.metric
  coordinate-system->metric-components
  coordinate-system->metric
  coordinate-system->inverse-metric
  literal-metric
  components->metric metric->components
  metric->inverse-components metric-over-map
  lower vector-field->oneform-field drop1
  raise oneform-field->vector-field raise1
  drop2 raise2 trace2down trace2up sharpen
  S2-metric]

 [emmy.calculus.map
  pullback-function pushforward-function
  differential-of-map differential
  pushforward-vector
  literal-manifold-map
  vector-field->vector-field-over-map
  form-field->form-field-over-map
  basis->basis-over-map
  pullback-form pullback-vector-field
  pullback]

 [emmy.calculus.vector-calculus
  Div Grad Curl Lap
  divergence curl gradient Laplacian]

 [emmy.calculus.vector-field
  vector-field?
  components->vector-field
  vector-field->components
  vf:zero
  literal-vector-field
  coordinate-basis-vector-field
  coordinate-system->vector-basis
  basis-components->vector-field
  vector-field->basis-components
  coordinatize evolution]

 [emmy.dual derivative]

 ;; Special Relativity

 [emmy.sr.boost
  make-four-tuple
  four-tuple->ct four-tuple->space
  proper-time-interval proper-space-interval
  general-boost general-boost2 extended-rotation]

 [emmy.sr.frames
  make-SR-coordinates SR-coordinates? SR-name make-SR-frame
  base-frame-maker
  the-ether boost-direction v:c coordinate-origin
  add-v:cs add-velocities]

 ;; Mechanics Namespaces

 [emmy.mechanics.lagrange
  ->L-state ->local ->state
  literal-Lagrangian-state
  Dt
  Euler-Lagrange-operator
  F->C
  Gamma Gamma-bar
  generalized-LE
  Lagrange-equations
  Lagrange-equations-first-order
  Lagrange-interpolation-function
  Lagrangian->energy Lagrangian->power-loss
  Lagrangian->state-derivative
  Lagrangian-action
  find-path
  linear-interpolants
  osculating-path
  r->s s->r p->r r->p
  state->t coordinate velocity acceleration
  coordinate-tuple velocity-tuple acceleration-tuple momentum-tuple]
 [emmy.matrix
  s->m
  m->s
  literal-matrix
  submatrix
  up->column-matrix
  column-matrix->up
  column-matrix->vector
  down->row-matrix
  row-matrix->down
  row-matrix->vector]
 [emmy.mechanics.hamilton
  ->H-state
  F->CT
  Hamilton-equations
  Hamiltonian
  Hamiltonian->state-derivative
  phase-space-derivative
  Lagrangian->Hamiltonian
  Legendre-transform
  Lie-transform
  Poisson-bracket
  compositional-canonical?
  iterated-map
  momentum
  polar-canonical
  standard-map
  qp-submatrix
  symplectic-transform?
  symplectic-unit
  time-independent-canonical?]
 [emmy.mechanics.rotation
  rotate-x-matrix rotate-y-matrix rotate-z-matrix
  angle-axis->rotation-matrix
  rotate-x-tuple rotate-y-tuple rotate-z-tuple
  Rx Ry Rz rotate-x rotate-y rotate-z
  Euler->M wcross->w]
 [emmy.numerical.ode
  evolve
  integrate-state-derivative
  state-advancer]
 [emmy.numerical.derivative D-numeric]
 [emmy.numerical.quadrature definite-integral]
 [emmy.numerical.unimin.brent
  brent-min brent-max]
 [emmy.numerical.multimin.nelder-mead nelder-mead]
 [emmy.numerical.unimin.golden
  golden-section-min golden-section-max]
 [emmy.numerical.minimize minimize multidimensional-minimize]
 [emmy.util.aggregate sum]
 [emmy.util.stream vector:generate]
 [emmy.special.elliptic elliptic-f]
 [emmy.special.factorial factorial]
 [emmy.value = compare
  numerical? kind kind-predicate principal-value])
