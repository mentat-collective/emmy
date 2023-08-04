((nil
  . ((cider-print-fn . "emmy.expression/expression->stream")
     (cider-default-cljs-repl . node)
     (cider-preferred-build-tool . clojure-cli)
     (cider-clojure-cli-aliases . ":test:cljs:nextjournal/clerk:dev")

     ;; Custom indentation:
     (eval . (put-clojure-indent 'sci-macro :defn))
     (eval . (put-clojure-indent 'careful-def 1)))))
