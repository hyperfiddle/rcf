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


(def testing-repl nil)

(macros/deftime
  (defn- start-testing-repl! []
    (assert (nil? testing-repl) "Already started")
    (alter-var-root #'testing-repl
                    (constantly
                      (macros/case
                        :clj  (do (dbg "STARTING TESTING REPL") (cljs-prepl))
                        :cljs nil)))
    (with-repl testing-repl (~'require '~'minitest))))

;; TODO: handle src-dirs
; (defn ns-paths [platform]
;   (->> (cp/classpath)
;        (filter (memfn ^java.io.File isDirectory))
;        (mapcat (fn [dir]
;                  (for [f (find-sources-in-dir dir)
;                        :let [decl (read-file-ns-decl f (:read-opts platform))]
;                        :when decl]
;                    [(name-from-ns-decl decl)
;                     (.getAbsolutePath f)])))
;        (into {})))

(defrecord CljsExecutor [opts store]
  ExecutorP
  (before-execute-suite
    [this ns->tests]
    (macros/case
      :clj (when-not testing-repl
             (dbg "NOOOOOOO")
             (start-testing-repl!)
             (with-repl testing-repl
               (~'require '~'minitest :reload) ;; TODO: do not reload minitest
               (~'use '~'cljs.core)
               (~'use '~'[cljs.repl :only (doc source error->str)]))))
    )
  (before-execute-namespace
    [this ns tests]
    (macros/case
      :clj  (let [res (with-repl testing-repl
                        ;; TODO: reload only if reloading in clj
                        (~'require '~ns :reload))]
              (dbg "REPL RESULT" res)))
    )
  (before-execute-block     [this ns tests])
  (before-execute-case      [this ns tests])
  (execute-case
    [this ns case]
    (macros/case
      :cljs (run-test-and-yield-report! ns case)
      :clj  nil #_(with-repl testing-repl
              (run-test-and-yield-report!
                '~ns
                ~(let [a assoc-in  u update-in  c case]
                   (-> c
                       (u [:tested   :form]    #(do `(quote ~%)))
                       (u [:expected :form]    #(do `(quote ~%)))
                       (a [:tested   :thunk]   (-> c :tested   :form as-thunk))
                       (a [:expected :thunk]   (-> c :expected :form as-thunk)))
                   )))))
  (after-execute-case       [this ns report])
  (after-execute-block      [this ns reports])
  (after-execute-namespace  [this ns reports])
  (after-execute-suite      [this ns->reports]))
