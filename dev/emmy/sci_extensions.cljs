(ns emmy.sci-extensions
  "SCI environment extensions, meant to apply to Emmy's Clerk documentation
  build."
  (:require [emmy.env.sci]
            [sci.core :as sci]
            [sci.ctx-store]))

(sci.ctx-store/swap-ctx!
 sci/merge-opts
 {:namespaces emmy.env.sci/context-opts})
