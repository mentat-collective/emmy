#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.sci
  (:refer-clojure :exclude [ns-map])
  (:require [emmy.env]
            [emmy.util :refer [copy-ns]]
            [sci.core :as sci]
            [sci.ctx-store])
  #?(:cljs (:require-macros [emmy.sci])))

(def namespaces
  "SCI namespace map. Consumers wishing to use a more
  minimal SCI environment should select their desired namespaces from this map."
  {'emmy.algebra.fold                   (copy-ns emmy.algebra.fold (sci/create-ns 'emmy.algebra.fold))
   'emmy.autodiff                        (copy-ns emmy.complex (sci/create-ns 'emmy.autodiff))

   'emmy.complex                        (copy-ns emmy.complex (sci/create-ns 'emmy.complex))
   'emmy.dual                           (copy-ns emmy.dual (sci/create-ns 'emmy.dual))
   'emmy.env                            (copy-ns emmy.env (sci/create-ns 'emmy.env))
   'emmy.expression                     (copy-ns emmy.expression (sci/create-ns 'emmy.expression))
   'emmy.function                       (copy-ns emmy.function (sci/create-ns 'emmy.function))
   'emmy.generic                        (copy-ns emmy.generic (sci/create-ns 'emmy.generic))
   'emmy.matrix                         (copy-ns emmy.matrix (sci/create-ns 'emmy.matrix))
   'emmy.modint                         (copy-ns emmy.modint (sci/create-ns 'emmy.modint))
   'emmy.numsymb                        (copy-ns emmy.numsymb (sci/create-ns 'emmy.numsymb))
   'emmy.operator                       (copy-ns emmy.operator (sci/create-ns 'emmy.operator))
   'emmy.pattern.consequence            (copy-ns emmy.pattern.consequence (sci/create-ns 'emmy.pattern.consequence))
   'emmy.pattern.match                  (copy-ns emmy.pattern.match (sci/create-ns 'emmy.pattern.match))
   'emmy.pattern.rule                   (copy-ns emmy.pattern.rule (sci/create-ns 'emmy.pattern.rule))
   'emmy.pattern.syntax                 (copy-ns emmy.pattern.syntax (sci/create-ns 'emmy.pattern.syntax))
   'emmy.polynomial                     (copy-ns emmy.polynomial (sci/create-ns 'emmy.polynomial))
   'emmy.polynomial.factor              (copy-ns emmy.polynomial.factor (sci/create-ns 'emmy.polynomial.factor))
   'emmy.polynomial.gcd                 (copy-ns emmy.polynomial.gcd (sci/create-ns 'emmy.polynomial.gcd))
   'emmy.polynomial.interpolate         (copy-ns emmy.polynomial.interpolate (sci/create-ns 'emmy.polynomial.interpolate))
   'emmy.polynomial.richardson          (copy-ns emmy.polynomial.richardson (sci/create-ns 'emmy.polynomial.richardson))
   'emmy.quaternion                     (copy-ns emmy.quaternion (sci/create-ns 'emmy.quaternion))
   'emmy.ratio                          (copy-ns emmy.ratio (sci/create-ns 'emmy.ratio))
   'emmy.rational-function              (copy-ns emmy.rational-function (sci/create-ns 'emmy.rational-function))
   'emmy.rational-function.interpolate  (copy-ns emmy.rational-function.interpolate (sci/create-ns 'emmy.rational-function.interpolate))
   'emmy.series                         (copy-ns emmy.series (sci/create-ns 'emmy.series))
   'emmy.simplify                       (copy-ns emmy.simplify (sci/create-ns 'emmy.simplify))
   'emmy.simplify.rules                 (copy-ns emmy.simplify.rules (sci/create-ns 'emmy.simplify.rules))
   'emmy.structure                      (copy-ns emmy.structure (sci/create-ns 'emmy.structure))
   'emmy.tape                           (copy-ns emmy.structure (sci/create-ns 'emmy.tape))

   'emmy.util                           (copy-ns emmy.util (sci/create-ns 'emmy.util))
   'emmy.value                          (copy-ns emmy.value (sci/create-ns 'emmy.value))
   'emmy.abstract.function              (copy-ns emmy.abstract.function (sci/create-ns 'emmy.abstract.function))
   'emmy.abstract.number                (copy-ns emmy.abstract.number (sci/create-ns 'emmy.abstract.number))
   'emmy.calculus.basis                 (copy-ns emmy.calculus.basis (sci/create-ns 'emmy.calculus.basis))
   'emmy.calculus.connection            (copy-ns emmy.calculus.connection (sci/create-ns 'emmy.calculus.connection))
   'emmy.calculus.coordinate            (copy-ns emmy.calculus.coordinate (sci/create-ns 'emmy.calculus.coordinate))
   'emmy.calculus.covariant             (copy-ns emmy.calculus.covariant (sci/create-ns 'emmy.calculus.covariant))
   'emmy.calculus.curvature             (copy-ns emmy.calculus.curvature (sci/create-ns 'emmy.calculus.curvature))
   'emmy.calculus.derivative            (copy-ns emmy.calculus.derivative (sci/create-ns 'emmy.calculus.derivative))
   'emmy.calculus.form-field            (copy-ns emmy.calculus.form-field (sci/create-ns 'emmy.calculus.form-field))
   'emmy.calculus.frame                 (copy-ns emmy.calculus.frame (sci/create-ns 'emmy.calculus.frame))
   'emmy.calculus.hodge-star            (copy-ns emmy.calculus.hodge-star (sci/create-ns 'emmy.calculus.hodge-star))
   'emmy.calculus.indexed               (copy-ns emmy.calculus.indexed (sci/create-ns 'emmy.calculus.indexed))
   'emmy.calculus.manifold              (copy-ns emmy.calculus.manifold (sci/create-ns 'emmy.calculus.manifold))
   'emmy.calculus.metric                (copy-ns emmy.calculus.metric (sci/create-ns 'emmy.calculus.metric))
   'emmy.calculus.map                   (copy-ns emmy.calculus.map (sci/create-ns 'emmy.calculus.map))
   'emmy.calculus.vector-calculus       (copy-ns emmy.calculus.vector-calculus (sci/create-ns 'emmy.calculus.vector-calculus))
   'emmy.calculus.vector-field          (copy-ns emmy.calculus.vector-field (sci/create-ns 'emmy.calculus.vector-field))
   'emmy.expression.analyze             (copy-ns emmy.expression.analyze (sci/create-ns 'emmy.expression.analyze))
   'emmy.expression.compile             (copy-ns emmy.expression.compile (sci/create-ns 'emmy.expression.compile))
   'emmy.expression.cse                 (copy-ns emmy.expression.cse (sci/create-ns 'emmy.expression.cse))
   'emmy.expression.render              (copy-ns emmy.expression.render (sci/create-ns 'emmy.expression.render))
   'emmy.mechanics.lagrange             (copy-ns emmy.mechanics.lagrange (sci/create-ns 'emmy.mechanics.lagrange))
   'emmy.mechanics.hamilton             (copy-ns emmy.mechanics.hamilton (sci/create-ns 'emmy.mechanics.hamilton))
   'emmy.mechanics.noether              (copy-ns emmy.mechanics.noether (sci/create-ns 'emmy.mechanics.noether))
   'emmy.mechanics.rigid                (copy-ns emmy.mechanics.rigid (sci/create-ns 'emmy.mechanics.rigid))
   'emmy.mechanics.rotation             (copy-ns emmy.mechanics.rotation (sci/create-ns 'emmy.mechanics.rotation))
   'emmy.mechanics.routhian             (copy-ns emmy.mechanics.routhian (sci/create-ns 'emmy.mechanics.routhian))
   'emmy.mechanics.time-evolution       (copy-ns emmy.mechanics.time-evolution (sci/create-ns 'emmy.mechanics.time-evolution))
   'emmy.numerical.derivative           (copy-ns emmy.numerical.derivative (sci/create-ns 'emmy.numerical.derivative))
   'emmy.numerical.minimize             (copy-ns emmy.numerical.minimize (sci/create-ns 'emmy.numerical.minimize))
   'emmy.numerical.ode                  (copy-ns emmy.numerical.ode (sci/create-ns 'emmy.numerical.ode))
   'emmy.numerical.quadrature           (copy-ns emmy.numerical.quadrature (sci/create-ns 'emmy.numerical.quadrature))
   'emmy.numerical.multimin.nelder-mead (copy-ns emmy.numerical.multimin.nelder-mead (sci/create-ns 'emmy.numerical.multimin.nelder-mead))
   'emmy.numerical.unimin.bracket       (copy-ns emmy.numerical.unimin.bracket (sci/create-ns 'emmy.numerical.unimin.bracket))
   'emmy.numerical.unimin.brent         (copy-ns emmy.numerical.unimin.brent (sci/create-ns 'emmy.numerical.unimin.brent))
   'emmy.numerical.unimin.golden        (copy-ns emmy.numerical.unimin.golden (sci/create-ns 'emmy.numerical.unimin.golden))
   'emmy.special.elliptic               (copy-ns emmy.special.elliptic (sci/create-ns 'emmy.special.elliptic))
   'emmy.special.factorial              (copy-ns emmy.special.factorial (sci/create-ns 'emmy.special.factorial))
   'emmy.sr.boost                       (copy-ns emmy.sr.boost (sci/create-ns 'emmy.sr.boost))
   'emmy.sr.frames                      (copy-ns emmy.sr.frames (sci/create-ns 'emmy.sr.frames))
   'emmy.util.aggregate                 (copy-ns emmy.util.aggregate (sci/create-ns 'emmy.util.aggregate))
   'emmy.util.def                       (copy-ns emmy.util.def (sci/create-ns 'emmy.util.def))
   'emmy.util.logic                     (copy-ns emmy.util.logic (sci/create-ns 'emmy.util.logic))
   'emmy.util.permute                   (copy-ns emmy.util.permute (sci/create-ns 'emmy.util.permute))
   'emmy.util.stream                    (copy-ns emmy.util.stream (sci/create-ns 'emmy.util.stream))})

(def config
  "Default sci context options required (currently only `:namespace`
  bindings) required to evaluate Emmy forms from inside of an SCI
  context. Pass these to `sci/init` to generate an sci context."
  {:namespaces namespaces

   ;; NOTE that these entries are required if you'd like to call the
   ;; `emmy.algebra.fold/kbk-n` macro, which generates code using
   ;; `Math/abs`. JVM and js forms are shown.
   :classes #?(:clj  {'java.lang.Math java.lang.Math}
               :cljs {'js goog/global :allow :all})})

(def context
  "sci context required to evaluate Emmy forms via SCI."
  (sci/init config))

(defn install!
  "Installs [[config]] into the shared SCI context store."
  []
  (sci.ctx-store/swap-ctx!
   sci/merge-opts
   config))
