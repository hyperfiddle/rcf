
(macros/deftime
  (def ^:dynamic ^:private *executing-cljs* false)

  ;; Taken and adapter from cljs.server.node to accept a repl-env as argument.
  (defn- default-opts [repl-env]
    (case repl-env
      cljs.repl.node/repl-env    {:host "localhost" :port 55555 #_49001}
      cljs.repl.browser/repl-env {:host "localhost" :port 55555 #_9000}
                                 {:host "localhost" :port 55555 #_50000}))

  (defonce ^:private envs (atom {}))

  (defn env-opts->key [{:keys [host port]}]
    [host port])

  (defn stale? [{:keys [socket] :as repl-env}]
    (if-let [sock (:socket @socket)]
      (.isClosed ^Socket sock)
      false))

  (defn- get-envs [repl-env env-opts]
    (let [env-opts (merge (default-opts repl-env) env-opts)
          k (env-opts->key env-opts)]
      (swap! envs
             #(cond-> %
                      (or (not (contains? % k))
                          (stale? (get-in % [k 0])))
                      (assoc k
                        [(apply repl-env (apply concat env-opts))
                         (env/default-compiler-env)])))
      (get @envs k)))

  (defn- prepl-opts []
    {:name           name
     :address        "127.0.0.1"
     :port           0 ;; picked at random
     :port-file      true
     :port-file-name (str "." name "-prepl-port")
     :env            (:js-env *context*)
     :accept         (let [sym (:prepl-fn (config))
                           ns  (some-> sym namespace symbol)]
                       (require (-> sym namespace symbol))
                       (-> sym resolve .toSymbol))})

  ;; TODO: implement support for figwheel prepl
  ;; See:  https://github.com/Olical/propel/blob/407ccf1ae507876e9a879239fac96380f2c1de2b/src/propel/core.clj#L66
  (defn- start-prepl! [opts]
    (clojure.core.server/start-server opts))

  ;; Taken from https://github.com/Olical/propel/blob/master/src/propel/util.clj
  (defmacro thread
    "Useful helper to run code in a thread but ensure errors are caught and
    logged correctly."
    [use-case & body]
    `(future
       (try
         ~@body
         (catch Throwable t#
           (error t# "From thread" (str "'" ~use-case "'"))))))

  (defn log [& msg]
    (apply println "[Minitest]" msg))

  (def error-lock (new Object))

  (defn error [err & msg]
    (locking error-lock
      (binding [*out* *err*]
        (apply log "Error:" msg)
        (-> err
            (cond-> (not (map? err)) (Throwable->map))
            (doto pprint)
            (clojure.main/ex-triage)
            (clojure.main/ex-str)
            (println)))))

  (defn- stop-prepl! [{:keys [name]}]
    (let []
      (clojure.core.server/stop-server name)))

  (defn cljs-prepl
    "The used js-env is dictated by (:js-env *context*)."
    []
    (let [writer (new PipedWriter)
          reader (new PipedReader writer)
          in>    (async/chan 32)
          out>   (async/chan 32)
          out-fn #(async/>!! out> %)
          opts   (prepl-opts)
          server (start-prepl! opts)
          env    (-> *context* :js-env name)]
      (thread (str "remote js prepl connection (" env ")")
        (with-open [reader reader]
          (clojure.core.server/remote-prepl
            (.. server getInetAddress getHostAddress) (.getLocalPort server)
            reader out-fn
            :valf identity)))
      (thread (str "js prepl eval loop (" env ")")
        (with-open [writer writer]
          (loop []
            (when-let [input (async/<!! in>)]
              (if (= input ::quit)
                (do (.close writer)
                    (.close reader))
                (let [s (str input "\n")]
                  (.write writer s)
                  (.flush writer)
                  (recur)))))))
      {:in in> :out out> :name (:name opts)}))

  ;; TODO: use partial
  (defn repl-exec!  [repl cmd]  (async/>!! (:in repl)  cmd))
  (defn repl-quit!  [repl]      (repl-exec! repl ::quit))
  (defn repl-result [repl]      (async/<!! (:out repl)))

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
                 (do (dbg "EXECUTING" e#) (repl-exec! repl# e#)
                     (repl-result repl#))))))))
