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

(macros/case
  :clj (do (defn with-out|   [out f]     #(binding [*out* out] (apply f %&)))
           (defn with-in|    [in  f]     #(binding [*in*  in]  (apply f %&)))
           (defn to-repl|    [repl f]    (with-out| (:in  repl) f))
           (defn from-repl|  [repl f]    (with-in|  (:out repl) f))

           ;; TODO: use partial
           (defn repl-exec!  [repl cmd]  ((to-repl|   repl prn)  cmd))
           (defn repl-quit!  [repl]      ((to-repl|   repl #(.close *out*))))
           (defn repl-result [repl]      ((from-repl| repl #(read))))

           (defmacro with-repl
             "Evaluates `body` in `repl`.

             Works on top of `clojure.tools.reader/syntax-quote`: in order to
             pass data between the host and the repl environments, forms marked
             with unquote reader macros (`~` & `~@`) are expanded."
             [repl & body]
             (let [expansion (binding [r/resolve-symbol identity]
                               (macroexpand `(r/syntax-quote ~body)))]
               `(let [repl# ~repl]
                  (last (for [e# ~expansion]
                          (do (repl-exec! repl# e#)
                              (repl-result repl#)))))))

           (defn cljs-prepl [repl-env]
             (let [in<         (PipedInputStream.)
                   in>  (PipedOutputStream. in<)
                   out<        (PipedInputStream.)
                   out> (PipedOutputStream. out<)
                   to-repl     (io/writer in>)
                   to-client   (io/writer out>)
                   from-repl   (PushbackReader. (io/reader out<))
                   from-client (PushbackReader. (io/reader in<))
                   repl        {:in to-repl :out from-repl}]
               (future (repl/repl
                         repl-env
                         ; :repl-requires    '[[cljs.repl :refer-macros [pst]]
                         ;                     #_[minitest  :refer        [test!]
                         ;                                :refer-macros [tests]]]
                         :quit-prompt       #()
                         :prompt            #()
                         :need-prompt       (constantly false)
                         :reader            #(rt/source-logging-push-back-reader
                                               from-client 1 "<MINITEST_REPL>")
                         :flush             (with-out| to-client flush)
                         :print             (with-out| to-client println)
                         :print-no-newline  (with-out| to-client print)
                         :eval              #(apply repl/eval-cljs %&)
                         :caught            (fn [e env opts]
                                              (println (ex-data e))
                                              (pst e)
                                              (pprint (.-stack e)))))
               (println "REQUIRE MINITEST IN CLJS"
                        (macros/case
                          :clj  :clj
                          :cljs :cljs))
               ; (repl-exec! repl '(require '[minitest :refer-macros [tests]]))
               (with-repl repl (require '[minitest :refer-macros [tests]]))
               repl))))

(def testing-repl-env
  (macros/case
    :clj  (node/repl-env)
    :cljs nil))

(def testing-repl
  (macros/case
    :clj  (cljs-prepl testing-repl-env)
    :cljs nil))

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

;; TODO
(defrecord CljsExecutor [opts store]
  ExecutorP
  (before-execute-suite
    [this ns->tests]
    (macros/case
      :clj  (let [req-stmt `(~'require ~@(map as-quote (keys ns->tests))
                                       :reload)
                  ; ns->path (ns-paths
                  ;            (macros/case
                  ;              :clj  clojure.tools.namespace.find/clj
                  ;              :cljs clojure.tools.namespace.find/cljs))
                  ]
              ; (doseq [pth (->> ns->tests keys (map ns->path))]
              ;   (println "pth" pth)
              ;   (println "(class pth)" (class pth))
              ;   (repl/load-file testing-repl-env pth))
              (println "REQ STMT" req-stmt)
              (let [res (with-repl testing-repl ~req-stmt)]
                (println "REPL RESULT" res))
              (Thread/sleep (* 10 1000)))
      :cljs (throw (ex-info "Can't run minitests for cljs in cljs for now" {}))))
  (before-execute-namespace [this ns tests])
  (before-execute-block     [this ns tests])
  (before-execute-case      [this ns tests])
  (execute-case             [this ns case])
  (after-execute-case       [this ns report])
  (after-execute-block      [this ns reports])
  (after-execute-namespace  [this ns reports])
  (after-execute-suite      [this ns->reports]))
