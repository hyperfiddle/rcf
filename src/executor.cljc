(declare run-test-and-yield-report!)

(defprotocol ExecutorP
  (before-execute-suite     [this ns->tests])
  (before-execute-namespace [this ns tests])
  (before-execute-block     [this ns tests])
  (before-execute-case      [this ns case])
  (execute-case             [this ns case])
  (after-execute-case       [this ns report])
  (after-execute-block      [this ns reports])
  (after-execute-namespace  [this ns reports])
  (after-execute-suite      [this ns->reports]))


(defrecord CljExecutor [opts store]
  ExecutorP
  (before-execute-suite     [this ns->tests])
  (before-execute-namespace [this ns tests])
  (before-execute-block     [this ns tests])
  (before-execute-case      [this ns tests])
  (execute-case             [this ns case] (run-test-and-yield-report! ns case))
  (after-execute-case       [this ns report])
  (after-execute-block      [this ns reports])
  (after-execute-namespace  [this ns reports])
  (after-execute-suite      [this ns->reports]))

(defn with-out|   [out f]     #(binding [*out* out] (apply f %&)))
(defn with-in|    [in  f]     #(binding [*in*  in]  (apply f %&)))
(defn to-repl|    [repl f]    (with-out| (:in  repl) f))
(defn from-repl|  [repl f]    (with-in|  (:out repl) f))

(defn repl-exec!  [repl cmd]  ((to-repl|   repl prn)  cmd))
(defn repl-quit!  [repl]      ((to-repl|   repl #(.close *out*))))
(defn repl-result [repl]      ((from-repl| repl #(read)))) ;; force no args

(defmacro with-repl
  "Evaluates `body` in `repl`.

  Works on top of `clojure.tools.reader/syntax-quote`: in order to
  pass data between the host and the repl environments, forms marked
  with unquote reader macros (`~` & `~@`) are expanded."
  [repl & body]
  (let [expansion (binding [r/resolve-symbol identity]
                    (macroexpand `(r/syntax-quote (do ~@body))))]
    `(let [repl# ~repl]
       (do (repl-exec! repl# ~expansion)
           (repl-result repl#)))))

(defn cljs-prepl [repl-env]
  (let [in<         (PipedInputStream.)   in>  (PipedOutputStream. in<)
        out<        (PipedInputStream.)   out> (PipedOutputStream. out<)
        to-repl     (io/writer in>)
        to-client   (io/writer out>)
        from-repl   (PushbackReader. (io/reader out<))
        from-client (PushbackReader. (io/reader in<))
        repl        (future
                      (repl/repl
                        repl-env
                        :quit-prompt      #()
                        :prompt           #()
                        :need-prompt      (constantly false)
                        :reader           #(rt/source-logging-push-back-reader
                                             from-client 1 "<TESTING_REPL>")
                        :flush            (with-out| to-client flush)
                        :print            (with-out| to-client println)
                        :print-no-newline (with-out| to-client print)
                        :eval             #(let [v (apply repl/eval-cljs %&)])))]
    {:in to-repl :out from-repl}))



(def testing-repl
  (cljs-prepl (node/repl-env)))

;; TODO
(defrecord CljsExecutor [opts store]
  ExecutorP
  (before-execute-suite
    [this ns->tests]
    (with-repl testing-repl
      ;; Arguments to require must be quoted. Offending spec: ns at line 1
      (doseq [~'ns '~(vec (keys ns->tests))]
        (require '~'ns))))
  (before-execute-namespace [this ns tests])
  (before-execute-block     [this ns tests])
  (before-execute-case      [this ns tests])
  (execute-case             [this ns case])
  (after-execute-case       [this ns report])
  (after-execute-block      [this ns reports])
  (after-execute-namespace  [this ns reports])
  (after-execute-suite      [this ns->reports]))
