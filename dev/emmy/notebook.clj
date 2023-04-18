^#:nextjournal.clerk
{:toc true
 :no-cache true
 :visibility :hide-ns}
(ns emmy.notebook
  {:nextjournal.clerk/auto-expand-results? true}
  (:refer-clojure
   :exclude [+ - * / zero? compare divide numerator denominator
             infinite? abs ref partial =])
  (:require [emmy.env :as e :refer :all]
            [mentat.clerk-utils.docs :as docs]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(clerk/eval-cljs
 ;; These aliases only apply inside this namespace!
 '(require '[reagent.core :as reagent]))

;; # Emmy
;;
;; A powerful computer algebra system written in Clojure(Script).

^{::clerk/visibility {:code :hide}}
(clerk/html
 [:center
  [:img
   {:width "200" :alt "Emmy logo"
    :src "https://raw.githubusercontent.com/mentat-collective/emmy/main/doc/img/logo.png"}]])

;; [![Build Status](https://github.com/mentat-collective/emmy/actions/workflows/kondo.yml/badge.svg?branch=main)](https://github.com/mentat-collective/emmy/actions/workflows/kondo.yml)
;; [![License](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/mentat-collective/emmy/blob/main/LICENSE)
;; [![cljdoc badge](https://cljdoc.org/badge/org.mentat/emmy)](https://cljdoc.org/d/org.mentat/emmy/CURRENT)
;; [![Clojars Project](https://img.shields.io/clojars/v/org.mentat/emmy.svg)](https://clojars.org/org.mentat/emmy)
;;
;; > The interactive documentation on this page was generated
;; > using [Clerk](https://github.com/nextjournal/clerk). Follow
;; > the [instructions in the
;; > README](https://github.com/mentat-collective/emmy/tree/main#interactive-documentation-via-clerk)
;; > to run and modify this notebook on your machine!
;; >
;; > See the [Github
;; > project](https://github.com/mentat-collective/emmy) for more
;; > details, and the [cljdoc
;; > page](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/readme) for
;; > detailed API documentation.
;;
;; ## What is Emmy?
;;
;; Emmy is a Clojure(Script) implementation of
;; the [scmutils](https://groups.csail.mit.edu/mac/users/gjs/6946/refman.txt)
;; system for math and physics investigations in the Clojure and ClojureScript
;; languages. Emmy provides facilities for

;; - [symbolic
;;   computation](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/data-types/symbolic-expressions),
;;   including state of the art $\TeX$ rendering and [expression
;;   simplification](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/simplification)
;; - [automatic](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/calculus/automatic-differentiation),
;;   [numerical](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/numerical-methods/numerical-derivative)
;;   and
;;   [symbolic](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/calculus/automatic-differentiation)
;;   differentiation
;; - [numerical integration and
;;   optimization](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/numerical-methods)
;; - investigations in [differential
;;   geometry](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/textbooks/functional-differential-geometry)
;;   and [Lagrangian and Hamiltonian
;;   mechanics](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/textbooks/structure-and-interpretation-of-classical-mechanics)

;; And implementations of many different [mathematical
;; objects](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/data-types), all
;; built on a tower of [generic, extensible mathematical
;; operations](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/basics/generics).

;; Scmutils is extensively used in the textbooks [The Structure and
;; Interpretation of Classical
;; Mechanics](https://mitpress.mit.edu/books/structure-and-interpretation-classical-mechanics-second-edition)
;; and [Functional Differential
;; Geometry](http://mitpress.mit.edu/books/functional-differential-geometry) by
;; G.J. Sussman and J. Wisdom.
;;
;; ## Quickstart
;;
;; Install `Emmy` into your Clojurescript project using the instructions at
;; its Clojars page:

;; [![Clojars Project](https://img.shields.io/clojars/v/org.mentat/emmy.svg)](https://clojars.org/org.mentat/emmy)
;;
;; Or grab the most recent code using a Git dependency:

^{::clerk/visibility {:code :hide}}
(docs/git-dependency
 "mentat-collective/emmy")

;; Require `emmy.env` in your Clojure(Script) namespace:

;; ```clj
;; (ns my-app
;;   (:require [emmy.env :as e :refer :all]))
;; ```

;; ## Guides
;;
;; These guides are currently woefully incomplete... we need more! Please hold
;; tight while we work on these...

;; ### Arithmetic / Numeric Tower



;; Math works as
;; expected (se [Generics](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/basics/generics)
;; for the full menu of operations), but notice that the numeric tower
;; includes [complex
;; numbers](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/data-types/complex),
;; and proper ratios in ClojureScript:

(- (* 7 (/ 1 2)) 2)

(->infix (asin -10))

;; ### Symbolic Arithmetic

;; Symbols are interpreted as abstract complex numbers, and arithmetic on them
;; generates symbolic expressions.

(square (sin (+ 'a 3)))

;; Render to an infix string
;; with [`->infix`](https://cljdoc.org/d/sicmutils/sicmutils/CURRENT/api/sicmutils.expression.render#-%3Einfix):

(->infix
 (square (sin (+ 'a 3))))

;; Or to $\TeX$
;; with [`->TeX`](https://cljdoc.org/d/sicmutils/sicmutils/CURRENT/api/sicmutils.expression.render#-%3ETeX):

(clerk/tex
 (->TeX
  (square (sin (+ 'a 3)))))

;; > Note the additional `nextjournal.clerk/tex` wrapper here, signaling to the
;; > notebook that we should render the string returned by `->TeX` as $\TeX$.
;;
;; Let's define a `render` function that does this for us:

(def render (comp clerk/tex ->TeX))

;; `simplify` can simplify symbolic expressions:

(let [expr (+ (square (sin 'x))
              (square (cos 'x)))]
  (render
   [expr (simplify expr)]))

;; If you name a symbol after a greek letter, it will render to that letter.
;; Capitalize the first letter to get the capital version of the character:

(render
 (+ 'Theta 'alpha))

;; Special suffixes like `dot`, `dotdot`, `prime`, `primeprime`, `var`, `vec`
;; and `tilde` will modify the symbol's infix or $\LaTeX$ representation. `_`
;; triggers a subscript, and the unicode character ↑ will trigger a superscript.

;; Here's a selection of examples:

(render
 (up
  'alphadot_beta
  'xdotdot
  'zetaprime_alphadot
  'alphaprimeprime_mubar
  'vbar
  'Pivec
  'alphatilde))

;; ### Automatic Differentiation

;; Use
;; the [`D`](https://cljdoc.org/d/sicmutils/sicmutils/CURRENT/api/sicmutils.calculus.derivative#D)
;; operator to perform [forward-mode automatic
;; differentiation](https://cljdoc.org/d/sicmutils/sicmutils/CURRENT/doc/calculus/automatic-differentiation).

(render
 ((D cube) 'x))

;; Generate the Taylor series expansion of a literal function `f` by
;; exponentiating the `D` operator:

(let [f (literal-function 'f)]
  (render
   (simplify
    (series:sum (((exp D) f) 'x) 5))))

;; ### Physics, Classical Mechanics

;; SICMUtils is based on the engine behind Sussman and Wisdom's [The Structure
;; and Interpretation of Classical
;; Mechanics](http://mitpress.mit.edu/books/structure-and-interpretation-classical-mechanics),
;; and has a built-in API for exploring Lagrangian and Hamiltonian mechanics.

;; Define a [Lagrangian](https://en.wikipedia.org/wiki/Lagrangian_mechanics) for
;; a central potential `U` acting on a particle with mass `m`:

(defn L-central-polar [m U]
  (fn [[_ [r] [rdot thetadot]]]
    (- (* 1/2 m
          (+ (square rdot)
             (square (* r thetadot))))
       (U r))))

;; and generate the two [Euler-Lagrange equations of
;; motion](https://en.wikipedia.org/wiki/Lagrangian_mechanics#Euler%E2%80%93Lagrange_equations_and_Hamilton's_principle)
;; for the `r` and `theta` coordinates:

(let [potential-fn (literal-function 'U)
      L     (L-central-polar 'm potential-fn)
      state (up (literal-function 'r)
                (literal-function 'theta))]
  (render
   (simplify
    (((Lagrange-equations L) state) 't))))

;; ## Emmy via SCI
;;
;; `Emmy` is compatible with [SCI, the Small Clojure
;; Interpreter](https://github.com/babashka/sci).
;;
;; To install `Emmy` into your SCI context, require
;; the [`emmy.sci`](https://cljdoc.org/d/org.mentat/emmy/CURRENT/api/emmy.sci)
;; namespace and call `emmy.sci/install!`:

;; ```clj
;; (ns myproject.sci-extensions
;;   (:require [emmy.sci]))

;; (emmy.sci/install!)
;; ```
;;
;; If you want more granular control, see the [cljdoc page for
;; `emmy.sci`](https://cljdoc.org/d/org.mentat/emmy/CURRENT/api/emmy.sci)
;; for an SCI config and distinct SCI namespace objects that you can piece
;; together.
;;
;; > Note that `Emmy` does not ship with a dependency on SCI, so you'll
;; > need to install your own version.
;;
;; ## Emmy via Clerk
;;
;; Using `Emmy` with Nextjournal's [Clerk](https://clerk.vision/) gives you the
;; ability to write notebooks like this one.
;;
;; Doing this requires that you generate a custom ClojureScript build for your
;; Clerk project. The easiest way to do this for an existing project is with
;; the [`clerk-utils` project](https://clerk-utils.mentat.org/). Follow the
;; instructions on the [`clerk-utils` guide for custom
;; ClojureScript](https://clerk-utils.mentat.org/#custom-clojurescript-builds).
;;
;; If this is your first time using Clerk, use the [`emmy/clerk` template
;; described below](#project-template) to generate a new project with all steps
;; described in ["Emmy via SCI"](#emmy-via-sci) already completed.
;;
;; ## Project Template
;;
;; `Emmy` includes a [`deps-new`](https://github.com/seancorfield/deps-new)
;; template called
;; [`emmy/clerk`](https://github.com/mentat-collective/emmy/tree/main/resources/emmy/clerk)
;; that makes it easy to configure a new Clerk project with everything described
;; in ["Emmy via SCI"](#emmy-via-sci) already configured.

;; First, install the [`deps-new`](https://github.com/seancorfield/deps-new)
;; tool:

;; ```sh
;; clojure -Ttools install io.github.seancorfield/deps-new '{:git/tag "v0.5.0"}' :as new
;; ```

;; To create a new Clerk project based on
;; [`emmy/clerk`](https://github.com/mentat-collective/emmy/tree/main/resources/emmy/clerk)
;; in a folder called `my-notebook-project`, run the following command:

^{::clerk/visibility {:code :hide}}
(clerk/md
 (format "
```sh
clojure -Sdeps '{:deps {io.github.mentat-collective/emmy {:git/sha \"%s\"}}}' \\
-Tnew create \\
:template emmy/clerk \\
:name myusername/my-notebook-project
```" (docs/git-sha)))

;; The README.md file in the generated project contains information on how to
;; develop within the new project.

;; If you have an existing Clerk notebook project and are considering adding
;; `Emmy`, you might consider
;; using [`emmy/clerk`](https://github.com/mentat-collective/emmy/tree/main/resources/emmy/clerk)
;; to get some ideas on how to structure your own project.


;; ## clj-kondo config

;; `Emmy` ships with a configuration that allows
;; [clj-kondo](https://github.com/clj-kondo/clj-kondo) to lint the library's
;; macros.

;; To install the exported linter configuration:

;; 1. Install clj-kondo using [these
;;    instructions](https://github.com/clj-kondo/clj-kondo/blob/master/doc/install.md).
;;    I highly recommend configuring [editor
;;    integration](https://github.com/clj-kondo/clj-kondo/blob/master/doc/editor-integration.md)
;;    for your text editor.

;; 2. If it doesn't exist yet, create a `.clj-kondo` folder in your project:

;; ```sh
;; mkdir .clj-kondo
;; ```

;; 3. Run `clj-kondo` using the following command. This will import the `emmy`
;; config and populate `clj-kondo`'s cache with linting information about all of
;; your dependencies:
;;
;; ```sh
;; # If you're using Leiningen:
;; clj-kondo --copy-configs --dependencies --lint$(lein classpath)"

;; # If you're using deps.edn:
;; clj-kondo --copy-configs --dependencies --lint "$(clojure -Spath)"
;; ```
;;
;; > The steps listed here mirror the [instructions in the clj-kondo
;; repo](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#importing).

;; ## Explore the Project
;;
;; Many of the namespaces in the project are written up as literate essays.
;; Expect more organization here in coming versions; for now, here is the full
;; list of namespace essays.

;; - [emmy.abstract.function](src/emmy/abstract/function.html)
;; - [emmy.abstract.number](src/emmy/abstract/number.html)
;; - [emmy.algebra.fold](src/emmy/algebra/fold.html)
;; - [emmy.calculus.basis](src/emmy/calculus/basis.html)
;; - [emmy.calculus.connection](src/emmy/calculus/connection.html)
;; - [emmy.calculus.coordinate](src/emmy/calculus/coordinate.html)
;; - [emmy.calculus.covariant](src/emmy/calculus/covariant.html)
;; - [emmy.calculus.curvature](src/emmy/calculus/curvature.html)
;; - [emmy.calculus.derivative](src/emmy/calculus/derivative.html)
;; - [emmy.calculus.form-field](src/emmy/calculus/form_field.html)
;; - [emmy.calculus.frame](src/emmy/calculus/frame.html)
;; - [emmy.calculus.hodge-star](src/emmy/calculus/hodge_star.html)
;; - [emmy.calculus.indexed](src/emmy/calculus/indexed.html)
;; - [emmy.calculus.manifold](src/emmy/calculus/manifold.html)
;; - [emmy.calculus.map](src/emmy/calculus/map.html)
;; - [emmy.calculus.metric](src/emmy/calculus/metric.html)
;; - [emmy.calculus.vector-calculus](src/emmy/calculus/vector_calculus.html)
;; - [emmy.calculus.vector-field](src/emmy/calculus/vector_field.html)
;; - [emmy.env.sci](src/emmy/env/sci.html)
;; - [emmy.env.sci.macros](src/emmy/env/sci/macros.html)
;; - [emmy.expression.analyze](src/emmy/expression/analyze.html)
;; - [emmy.expression.compile](src/emmy/expression/compile.html)
;; - [emmy.expression.cse](src/emmy/expression/cse.html)
;; - [emmy.expression.render](src/emmy/expression/render.html)
;; - [emmy.mechanics.hamilton](src/emmy/mechanics/hamilton.html)
;; - [emmy.mechanics.lagrange](src/emmy/mechanics/lagrange.html)
;; - [emmy.mechanics.noether](src/emmy/mechanics/noether.html)
;; - [emmy.mechanics.rigid](src/emmy/mechanics/rigid.html)
;; - [emmy.mechanics.rotation](src/emmy/mechanics/rotation.html)
;; - [emmy.mechanics.routhian](src/emmy/mechanics/routhian.html)
;; - [emmy.mechanics.time-evolution](src/emmy/mechanics/time_evolution.html)
;; - [emmy.numerical.derivative](src/emmy/numerical/derivative.html)
;; - [emmy.numerical.minimize](src/emmy/numerical/minimize.html)
;; - [emmy.numerical.multimin.nelder-mead](src/emmy/numerical/multimin/nelder_mead.html)
;; - [emmy.numerical.ode](src/emmy/numerical/ode.html)
;; - [emmy.numerical.quadrature](src/emmy/numerical/quadrature.html)
;; - [emmy.numerical.quadrature.adaptive](src/emmy/numerical/quadrature/adaptive.html)
;; - [emmy.numerical.quadrature.boole](src/emmy/numerical/quadrature/boole.html)
;; - [emmy.numerical.quadrature.bulirsch-stoer](src/emmy/numerical/quadrature/bulirsch_stoer.html)
;; - [emmy.numerical.quadrature.common](src/emmy/numerical/quadrature/common.html)
;; - [emmy.numerical.quadrature.infinite](src/emmy/numerical/quadrature/infinite.html)
;; - [emmy.numerical.quadrature.midpoint](src/emmy/numerical/quadrature/midpoint.html)
;; - [emmy.numerical.quadrature.milne](src/emmy/numerical/quadrature/milne.html)
;; - [emmy.numerical.quadrature.riemann](src/emmy/numerical/quadrature/riemann.html)
;; - [emmy.numerical.quadrature.romberg](src/emmy/numerical/quadrature/romberg.html)
;; - [emmy.numerical.quadrature.simpson](src/emmy/numerical/quadrature/simpson.html)
;; - [emmy.numerical.quadrature.simpson38](src/emmy/numerical/quadrature/simpson38.html)
;; - [emmy.numerical.quadrature.substitute](src/emmy/numerical/quadrature/substitute.html)
;; - [emmy.numerical.quadrature.trapezoid](src/emmy/numerical/quadrature/trapezoid.html)
;; - [emmy.numerical.roots.bisect](src/emmy/numerical/roots/bisect.html)
;; - [emmy.numerical.unimin.bracket](src/emmy/numerical/unimin/bracket.html)
;; - [emmy.numerical.unimin.brent](src/emmy/numerical/unimin/brent.html)
;; - [emmy.numerical.unimin.golden](src/emmy/numerical/unimin/golden.html)
;; - [emmy.pattern.consequence](src/emmy/pattern/consequence.html)
;; - [emmy.pattern.match](src/emmy/pattern/match.html)
;; - [emmy.pattern.rule](src/emmy/pattern/rule.html)
;; - [emmy.pattern.syntax](src/emmy/pattern/syntax.html)
;; - [emmy.polynomial.exponent](src/emmy/polynomial/exponent.html)
;; - [emmy.polynomial.factor](src/emmy/polynomial/factor.html)
;; - [emmy.polynomial.gcd](src/emmy/polynomial/gcd.html)
;; - [emmy.polynomial.impl](src/emmy/polynomial/impl.html)
;; - [emmy.polynomial.interpolate](src/emmy/polynomial/interpolate.html)
;; - [emmy.polynomial.richardson](src/emmy/polynomial/richardson.html)
;; - [emmy.rational-function.interpolate](src/emmy/rational_function/interpolate.html)
;; - [emmy.series.impl](src/emmy/series/impl.html)
;; - [emmy.simplify.rules](src/emmy/simplify/rules.html)
;; - [emmy.special.elliptic](src/emmy/special/elliptic.html)
;; - [emmy.special.factorial](src/emmy/special/factorial.html)
;; - [emmy.sr.boost](src/emmy/sr/boost.html)
;; - [emmy.sr.frames](src/emmy/sr/frames.html)
;; - [emmy.util.aggregate](src/emmy/util/aggregate.html)
;; - [emmy.util.def](src/emmy/util/def.html)
;; - [emmy.util.logic](src/emmy/util/logic.html)
;; - [emmy.util.permute](src/emmy/util/permute.html)
;; - [emmy.util.stopwatch](src/emmy/util/stopwatch.html)
;; - [emmy.util.stream](src/emmy/util/stream.html)
;; - [emmy.util.vector-set](src/emmy/util/vector_set.html)

;; ## Who is using Emmy?

;; The following projects use Emmy:

;; - [Emmy-Viewers](https://github.com/mentat-collective/emmy-viewers), experimental visualizations using Emmy
;; - The coming-soon [Road to Reality Essays](https://github.com/mentat-collective/road-to-reality), based on https://roadtoreality.substack.com
;; - [Executable version of SICM](https://github.com/mentat-collective/sicm-book)
;; - [Executable version of FDG](https://github.com/mentat-collective/fdg-book)
;; - [SICM Exercises in Clojure](https://github.com/mentat-collective/sicm-clj-exercises)
;; - [@kloimhardt](https://github.com/kloimhardt) has integrated Emmy into his
;;   [Scratch](https://scratch.mit.edu)-like, blocks-based language at
;;   [clj-tiles](https://github.com/kloimhardt/clj-tiles). See
;;   [this demo](https://kloimhardt.github.io/cljtiles.html?page=freeparticle) for
;;   a lovely example of Lagrangian Mechanics in `clj-tiles`.
;;
;; If you want to show off your use of Emmy, please [file a
;; ticket](https://github.com/mentat-collective/emmy/issues/new) and let us
;; know!

;; ## Thanks and Support

;; To support this work and my other open source projects, consider sponsoring
;; me via my [GitHub Sponsors page](https://github.com/sponsors/sritchie). Thank
;; you to my current sponsors!

;; For more information on me and my work, visit https://samritchie.io.

;; ## License

;; Copyright © 2022-2023 Sam Ritchie and Colin Smith.

;; Distributed under the [MIT
;; License](https://github.com/mentat-collective/emmy/blob/main/LICENSE).
;; See [LICENSE](https://github.com/mentat-collective/emmy/blob/main/LICENSE).
