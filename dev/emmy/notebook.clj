^#:nextjournal.clerk
{:toc true
 :no-cache true
 :visibility :hide-ns}
(ns emmy.notebook
  {:nextjournal.clerk/auto-expand-results? true}
  (:require [mentat.clerk-utils.docs :as docs]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(clerk/eval-cljs
 ;; These aliases only apply inside this namespace.
 '(require '[reagent.core :as reagent]))

;; # Emmy
;;
;; TODO.

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
;; Emmy is... TODO.
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

;; TODO quickstart.

;; ## Guides
;;
;; TODO.

;; ## Literate Repository

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
;; [`emmy/clerk`](https://github.com/mentat-collective/clerk-utils/tree/main/resources/clerk_utils/custom)
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

;; ## Who is using Emmy?

;; The following projects use Emmy:

;; - TODO

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
