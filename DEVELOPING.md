## Dev Dependencies

- [node.js](https://nodejs.org/en/)
- The [clojure command line tool](https://clojure.org/guides/install_clojure)
- [Babashka](https://github.com/babashka/babashka#installation)

## Github Pages, Docs Notebook

The project's [Github Pages site](https://emmy.mentat.org) hosts an
interactive [Clerk](https://github.com/nextjournal/clerk) notebook demonstrating
the library's use.

### Local Notebook Dev

Start a Clojure process however you like, and run `(user/serve!)` to run the
Clerk server. This command should open up `localhost:7777`.

Alternatively, run

```sh
bb clerk-watch
```

### Static Build

To test the static build locally:

```
bb publish-local
```

This will generate the static site in `public/build`, start a development http
server and open up a browser window (http://127.0.0.1:8080/) with the production
build of the documentation notebook.

### GitHub Pages

To build and release to Github Pages:

```
bb release-gh-pages
```

This will ship the site to https://emmy.mentat.org.

## Publishing to Clojars

The template for the project's `pom.xml` lives at
[`template/pom.xml`](https://github.com/mentat-collective/emmy/blob/main/template/pom.xml).

To create a new release:

- Update the version in
  [resources/EMMY_VERSION](https://github.com/mentat-collective/emmy/blob/main/resources/EMMY_VERSION)
- Make a new [Github
  Release](https://github.com/mentat-collective/emmy/releases) with tag
  `v<the-new-version>`.

Submitting the release will create the new tag and trigger the following
command:

```
bb release
```

The new release will appear on Clojars.

## Linting

Code is linted with [`clj-kondo`](https://github.com/clj-kondo/clj-kondo):

```
bb lint
```
