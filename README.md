<h1 align="center">
<img src="https://raw.githubusercontent.com/mentat-collective/emmy/main/doc/img/logo.png" width="250" alt="Emmy logo" title="Emmy" />
</h1>

<div align="center">

[![Build Status][build-status]][build-status-url]
[![License][license]][license-url]
[![Codecov branch][codecov]][codecov-url]
[![cljdoc badge][cljdoc]][cljdoc-url]
[![Clojars Project][clojars]][clojars-url]
[![Discord Shield][discord]][discord-url]

</div>

Emmy is a Clojure(Script) implementation of the [scmutils][scmutils-refman-url]
system for math and physics investigations in the Clojure and ClojureScript
languages. Emmy provides facilities for

- [symbolic
  computation](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/data-types/symbolic-expressions),
  including state of the art TeX rendering and [expression
  simplification](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/simplification)
- [automatic](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/calculus/automatic-differentiation),
  [numerical](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/numerical-methods/numerical-derivative)
  and
  [symbolic](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/calculus/automatic-differentiation)
  differentiation
- [numerical integration and
  optimization](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/numerical-methods)
- investigations in [differential
  geometry](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/textbooks/functional-differential-geometry)
  and [Lagrangian and Hamiltonian
  mechanics](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/textbooks/structure-and-interpretation-of-classical-mechanics)

And implementations of many different [mathematical
objects](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/data-types), all built
on a tower of [generic, extensible mathematical
operations](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/basics/generics).

Scmutils is extensively used in the textbooks [The Structure and Interpretation
of Classical Mechanics][sicm-book-url] and [Functional Differential
Geometry][fdg-book-url] by G.J. Sussman and J. Wisdom.

> :wave: Need help getting started? Say hi on [Clojurians
> Slack](http://clojurians.net/) in [#emmy][emmy-slack-url].

## Quickstart

> **Note**
> Emmy is best experienced in an interactive environment like the
> [REPL](https://clojure.org/guides/repl/introduction). We [support many
> environments](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/basics/how-to-use-emmy)
> with rich support for [TeX](https://en.wikipedia.org/wiki/TeX) rendering and
> plotting.

Install `Emmy` into your Clojure(Script) project using the instructions at its
Clojars page:

[![Clojars Project][clojars]][clojars-url]

Or grab the most recent code using a Git dependency:

```clj
;; Replace $GIT_SHA with the most recent commit:
{io.github.mentat-collective/emmy
  {:git/sha "$GIT_SHA"}}
```

Require `emmy.env` in your Clojure(Script) namespace:

```clj
(ns my-app
  (:require [emmy.env :as e :refer :all]))
```

Or clone this repository:

```sh
git clone git@github.com:mentat-collective/emmy.git
cd emmy
```

then install [the Clojure command line
tool](https://clojure.org/guides/install_clojure) and run the following command
to launch a REPL with `emmy.env` already loaded:

```sh
clj -M:test:dev:repl
```

Math works as expected (see
[Generics](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/basics/generics) for
the full menu of operations), but notice that the numeric tower includes
[complex
numbers](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/data-types/complex),
and proper ratios in ClojureScript:

```clj
(- (* 7 (/ 1 2)) 2)
;;=> 3/2

(asin -10)
;;=> #emmy/complex [-1.5707963267948966 2.9932228461263786]
```

Symbols are interpreted as abstract complex numbers, and arithmetic on them
generates symbolic expressions. You can render these with
[`->TeX`](https://cljdoc.org/d/org.mentat/emmy/CURRENT/api/emmy.expression.render#->TeX)
and
[`->infix`](https://cljdoc.org/d/org.mentat/emmy/CURRENT/api/emmy.expression.render#->infix):

```clojure
(def render (comp ->infix simplify))

(square (sin (+ 'a 3)))
;;=> (expt (sin (+ a 3)) 2)

(render (square (sin (+ 'a 3))))
;;=> "sin²(a + 3)"
```

Use the
[`D`](https://cljdoc.org/d/org.mentat/emmy/CURRENT/api/emmy.calculus.derivative#D)
operator to perform [forward-mode automatic
differentiation](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/calculus/automatic-differentiation)
and
[`simplify`](https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/simplification) to
collapse symbolic expressions into tidy form:

```clojure
((D cube) 'x)
;;=>  (+ (* x (+ x x)) (* x x))

(simplify ((D cube) 'x))
;;=> (* 3 (expt x 2))

(->infix
 (simplify ((D cube) 'x)))
;;-> "3 x²"
```

Emmy is based on the engine behind [The Structure and Interpretation of
Classical Mechanics][sicm-book-url], and has a built-in API for exploring
Lagrangian and Hamiltonian mechanics.

Define a [Lagrangian](https://en.wikipedia.org/wiki/Lagrangian_mechanics) for a
central potential `U` acting on a particle with mass `m`:

```clojure
(defn L-central-polar [m U]
  (fn [[_ [r] [rdot thetadot]]]
    (- (* 1/2 m
          (+ (square rdot)
             (square (* r thetadot))))
       (U r))))
```

and generate the two [Euler-Lagrange equations of
motion](https://en.wikipedia.org/wiki/Lagrangian_mechanics#Euler–Lagrange_equations_and_Hamilton's_principle)
for the `r` and `theta` coordinates:

```clojure
(let [potential-fn (literal-function 'U)
      L     (L-central-polar 'm potential-fn)
      state (up (literal-function 'r)
                (literal-function 'theta))]
  (render
   (((Lagrange-equations L) state) 't)))
;;=> "down(- m r(t) (Dθ(t))² + m D²r(t) + DU(r(t)), m (r(t))² D²θ(t) + 2 m r(t) Dr(t) Dθ(t))"
```

There is so much more! This is a dense library, and lots of documentation
remains to be written. Some suggested next steps, for now:

- Open up the live, interactive [Emmy tutorial on
  Nextjournal](https://nextjournal.com/try/samritchie/sicmutils), play with the
  examples above and start to explore on your own.
- Read the [Emmy Reference Manual][refman-url] ("refman") for inspiration. All
  of the code snippets in the refman will work in the [Nextjournal
  environment](https://nextjournal.com/try/samritchie/sicmutils). Use the two
  together.
- Visit our [CLJDocs][cljdoc-url] page for an introduction and detailed
  documentation
- Watch Colin's ["Physics in Clojure"][physics-in-clj-talk-url] talk for an overview
  of Emmy and its implementation
- Watch Sam's ["Emmy: Moldable Physics and Lispy Microworlds"][emmy-talk-url]
  talk for the motivation behind the visual extensions to Emmy
- Visit the HTML version of [Structure and Interpretation of Classical
  Mechanics](https://tgvaughan.github.io/sicm/). Many of the SICM exercises have
  been worked using Emmy; they live at [this Nextjournal
  page](https://nextjournal.com/sicm).

## Clerk Support

If you want to use `Emmy` with [Clerk][clerk-url], check out the [`emmy/clerk`
template][emmy-clerk-template-url]. This [`deps-new`][deps-new-url] template
will generate a Clerk project for you, fully configured to use `Emmy` in your
Clerk notebooks.

## Interactive Documentation via Clerk

The project's [interactive documentation](https://emmy.mentat.org) was generated
using Nextjournal's [Clerk][clerk-url]. If you'd like to edit or play with the
documentation, you'll need to install

- [node.js](https://nodejs.org/en/)
- The [Clojure command line tool](https://clojure.org/guides/install_clojure)
- [Babashka](https://github.com/babashka/babashka#installation)

Next, clone the repository:

```bash
git clone git@github.com:mentat-collective/emmy.git
cd emmy
```

Run this command in the cloned repository:

```sh
bb clerk-watch
```

This will open a browser window to `http://localhost:7777` with the contents of
the documentation notebook. Any edits you make to `dev/emmy/notebook.clj`
will be picked up and displayed in the browser on save.

## Background

[SICM][sicm-book-url] and [FDG][fdg-book-url] can be thought of as spiritual
successors to [The Structure and Interpretation of Computer
Programs][sicp-book-url], a very influential text—as I can attest, since
carefully reading this book in my 30s changed my life as a programmer. To see
the same techniques applied to differential geometry and physics is an
irresistible lure.

Scmutils is an excellent system, but it is written in an older variant of LISP
(Scheme) and is tied to a particular implementation of Scheme—MIT/GNU Scheme.
(There is a [port to Guile][gscm-url], but due to the fact that Guile does not
support MIT Scheme's [apply
hooks](https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Application-Hooks.html)
some glue code is required to run examples from the book in that environment.)

Having the system in Clojure offers a number of advantages. It is not necessary
to obtain or prepare a MIT/GNU Scheme executable to execute: only a Java runtime
is required. It does not require the X Window System for graphics, as MIT Scheme
does. All of the standard tooling for Java and Clojure become available, and
this is a lot compared to what we get with MIT/GNU scheme. Clojure support is
now extensive in any number of editors and IDEs. Even better, you can interact
with the system in the context of a [Jupyter notebook](./jupyter).

You can invoke the system from within Java or JavaScript code or use any Java or
JS packages you like together with the mathematics system. It's my hope that
continuing this project will extend the reach of SICM and FDG by allowing
experimentation and collaboration with them in modern environments.

## Citing Emmy

To cite this repository, see the "Cite this Repository" link on the top right of
the Github page. Citation information is generated from
[`CITATION.cff`](CITATION.cff).

Here is the generated BibTeX entry:

```
@software{Ritchie_Emmy_Functional_Computer_2016},
author = {Ritchie, Sam and Smith, Colin},
license = {GPL-3.0},
month = {4},
title = {{Emmy: Functional Computer Algebra in Clojure}},
url = {https://github.com/mentat-collective/emmy},
version = {0.31.0},
year = {2016}
```

In the above BibTeX entry, the version number is intended to be that from
[resources/EMMY_VERSION](./resources/EMMY_VERSION), and the year corresponds to
the project's open-source release.

## License

Copyright © 2016-2023 Colin Smith, Sam Ritchie.

Distributed under the [GPL v3](LICENSE) license. See [LICENSE](LICENSE).

[build-status-url]: https://github.com/mentat-collective/emmy/actions?query=workflow%3A%22Clojure+CI%22
[build-status]: https://github.com/mentat-collective/emmy/workflows/Clojure%20CI/badge.svg?branch=main
[clerk-url]: https://clerk.vision
[cljdoc-url]: https://cljdoc.org/d/org.mentat/emmy/CURRENT
[cljdoc]: https://cljdoc.org/badge/org.mentat/emmy
[clojars-url]: https://clojars.org/org.mentat/emmy
[clojars]: https://img.shields.io/clojars/v/org.mentat/emmy.svg
[codecov-url]: https://codecov.io/github/mentat-collective/emmy
[codecov]: https://img.shields.io/codecov/c/github/mentat-collective/emmy/main.svg?maxAge=3600
[deps-new-url]: https://github.com/seancorfield/deps-new
[discord-url]: https://discord.gg/hsRBqGEeQ4
[discord]: https://img.shields.io/discord/731131562002743336?style=flat&colorA=000000&colorB=000000&label=&logo=discord
[emmy-clerk-template-url]: https://github.com/mentat-collective/emmy/tree/main/resources/emmy/clerk
[emmy-slack-url]: https://clojurians.slack.com/archives/C01ECA9AA74
[emmy-talk-url]: https://www.youtube.com/watch?v=B9kqD8vBuwU
[fdg-book-url]: https://mitpress.mit.edu/9780262019347/functional-differential-geometry/
[gscm-url]: http://www.cs.rochester.edu/~gildea/guile-scmutils/
[license-url]: LICENSE
[license]: https://img.shields.io/badge/license-GPLv3-brightgreen.svg
[physics-in-clj-talk-url]: https://www.youtube.com/watch?v=7PoajCqNKpg
[refman-url]: https://cljdoc.org/d/org.mentat/emmy/CURRENT/doc/reference-manual
[scmutils-refman-url]: https://groups.csail.mit.edu/mac/users/gjs/6946/refman.txt
[sicm-book-url]: https://mitpress.mit.edu/9780262028967/structure-and-interpretation-of-classical-mechanics/
[sicp-book-url]: https://mitpress.mit.edu/9780262510875/structure-and-interpretation-of-computer-programs/
