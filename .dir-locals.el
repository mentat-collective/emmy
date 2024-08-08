((nil
  . ((cider-print-fn . "emmy.expression/expression->stream")
     (cider-default-cljs-repl . node)
     (cider-preferred-build-tool . clojure-cli)
     (cider-clojure-cli-aliases . ":test:cljs:nextjournal/clerk:dev")

     ;; Custom indentation:
     (eval . (progn
               ;; the require here avoids projectile errors:
               ;;   "Symbol’s function definition is void: put-clojure-indent"
               (require 'clojure-mode)
               (require 'cider)
               (put-clojure-indent 'sci-macro :defn)
               (put-clojure-indent 'careful-def 1))))))