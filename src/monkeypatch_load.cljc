
(defn clear-tests!        [a & nss]    (swap! a #(apply dissoc % nss)))
(defn add-tests!          [a ns blocs] (swap! a update ns concat blocs))
(defn store-tests!        [ns blocs]   (add-tests! *tests*            ns blocs))
(defn process-after-load! [ns blocs]   (add-tests! *tests-to-process* ns blocs))

(defn process-on-load-tests-now! []
  (assert *currently-loading*)
  (try
    (let [conf (config)]
      (when (:store conf) (run! #(apply store-tests! %) @*tests-to-process*))
      (when (:run conf)   (run-execute-report! :suite @*tests-to-process*)))
    (finally
      (reset! *tests-to-process* nil))))

;; Clojure
(macros/deftime
  (defn- clj-core-load-around-hook [orig-load & paths]
    (dbg "LOADING" paths)
    (if cljs.repl/*repl-env*
      (apply orig-load paths)
      (with-contexts {:exec-mode :load}
          (binding [*currently-loading* true
                    *tests-to-process* (atom nil)]
            (let [conf        (config)
                  path->ns    (->> (all-ns)
                                   (mapv (juxt
                                           (->| str @#'clojure.core/root-resource)
                                           identity))
                                   (into {}))
                  nss         (map (->|  path->ns str symbol) paths)
                  load-result (do (apply clear-tests! *tests* nss)
                                  (ensuring-runner+executors+reporter ;; TODO: necessary ?
                                    (apply orig-load paths)))]
              (when (and (or (:store conf) (:run conf))
                         (> (count @*tests-to-process*) 0))
                (process-on-load-tests-now!))
              load-result)))))

  (defn- apply-patch-to-clojure-core-load []
    (add-hook #'clojure.core/load
              #'clj-core-load-around-hook)))

;; ClojureScript
(macros/deftime
  ;; We deal directly with JavaScript because reusing the current cljs
  ;; compilation settings is tricky.
  (defn- require-js [ns]
    (cljs/emitln "goog.require('" (cljs/munge ns) "');"))

  (defmacro ^:private with-js-block [& emitting-body]
    `(do  (cljs/emitln "{")  ~@emitting-body  (cljs/emitln "}")))

  (defmacro ^:private try-js [expr-emitter & {:keys [finally]}]
    `(do (cljs/emitln "try {")
         ~expr-emitter
         (cljs/emitln "} finally {")
         ~finally
         (cljs/emitln "}")))

  (defmacro ^:private  binding-js [[var-name expr] & emitting-body]
    `(let [js-var#   (cljs/munge '~var-name)
           js-val#   ~expr
           orig-var# (-> (str "_original_" js-var#)
                         cljs/munge
                         (str/replace #"[.]" "_")
                         gensym)]
       (with-js-block ;; guard from changing vars above...
         (cljs/emitln "const " orig-var#  " = " js-var# ";")
         (cljs/emitln          js-var#    " = " js-val# ";")
         ;; ...and being changed by assignments below with try's own block scope
         (try-js ~@emitting-body
                 :finally (cljs/emitln js-var# " = " orig-var# ";"))
         ;; See: https://stackoverflow.com/a/39798496
         )))

  (defn- separate [pred coll] [(filter pred coll) (remove pred coll)])
  (defn- rotate   [x]         (apply map vector x))

  (defn- separate-load-libs [these-deps [libs seen reloads deps ns-name]]
    (let [tdeps          (set these-deps)
          odeps          (apply disj (set deps) tdeps)
          [these others] (->> (map #(separate tdeps %)
                                   [seen reloads deps])
                              rotate)]
      `[[~(select-keys libs tdeps) ~@these  ~ns-name]
        [~(select-keys libs odeps) ~@others ~ns-name]]))

  (defn- cljs-compiler-load-libs-around-hook [orig-load-libs
                                              & [libs seen reloads deps ns-name
                                                 :as args]]
    (if (= ns-name 'minitest)
      (apply orig-load-libs args)
      (let [[mini others :as x] (separate-load-libs ['minitest] args)
            mini-deps (nth mini 3)]
        (binding [*out* (io/writer java.lang.System/out)]
          (dbg "NSNAME" ns-name)
          (dbg "MINI" mini)
          (dbg "OTHERS" others)
          (dbg ""))
        (when (seq mini-deps)       (apply orig-load-libs mini))
        (let [emitted (with-out-str (apply orig-load-libs others))]
          (when (seq emitted)
            (when-not (seq mini-deps) (require-js 'minitest))
            (binding-js [minitest.*currently-loading* true]
              (print emitted)))))))

  (defn- apply-patch-to-cljs-compiler-load-libs []
    (add-hook #'cljs.compiler/load-libs
              #'cljs-compiler-load-libs-around-hook)))
