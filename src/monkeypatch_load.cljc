
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
    (cljsc/emitln "goog.require('" (cljsc/munge ns) "');"))

  (defmacro ^:private with-js-block [& emitting-body]
    `(do  (cljsc/emitln "{")  ~@emitting-body  (cljsc/emitln "}")))

  (defmacro ^:private try-js [expr-emitter & {:keys [finally]}]
    `(do (cljsc/emitln "try {")
         ~expr-emitter
         (cljsc/emitln "} finally {")
         ~finally
         (cljsc/emitln "}")))

  (defmacro ^:private  binding-js [[var-name expr] & emitting-body]
    `(let [js-var#   '~(cljsc/munge var-name) ;; Keeps .
           js-val#   ~expr
           orig-var# '~(munge (str "original_" var-name))] ;; Converts . to _
       (with-js-block ;; guards from changing vars above...
         (cljsc/emitln "const " orig-var#  " = " js-var# ";")
         (cljsc/emitln          js-var#    " = " js-val# ";")
         ;; ...and being changed by assignments below with try's own block scope
         (try-js ~@emitting-body
                 :finally (cljsc/emitln js-var# " = " orig-var# ";"))
         ;; See: https://stackoverflow.com/a/39798496
         )))

  (defn- cljs-compiler-load-libs-around-hook [orig-load-libs
                                              & [libs seen reloads deps ns-name
                                                 :as args]]
    ; (binding [*out* (io/writer java.lang.System/out)]
    ;   (pprint {:ns     ns-name
    ;            :args   (let [[libs seen reloads deps ns-name] args]
    ;                      {:libs libs :seen seen :reloads reloads :deps deps
    ;                       :ns-name ns-name})})
    ;   (orig-load-libs '{minitest minitest} nil nil '[minitest] 'cljs.user)
    ;   #_(when-not (= ns-name 'minitest)
    ;     #_(pprint (ana/empty-env))
    ;     #_(pprint (ana/analyze
    ;               (assoc (ana/empty-env)
    ;                 :ns (cljs.analyzer/get-namespace (ana/current-ns)))
    ;               (macroexpand `(cljs.core/require '~'minitest))
    ;               nil
    ;               (some-> (ana/current-state) deref :options))))
    ;   ; (pprint (seq (.getStackTrace (ex-info "" {}))))
    ;   (dbg "")
    ;   (orig-load-libs '{minitest minitest} nil nil '[minitest] 'cljs.user)
    ;   (apply orig-load-libs args))

    ; (orig-load-libs '{minitest minitest} nil nil '[minitest] 'cljs.user)
    ; (apply orig-load-libs args)

    (if (= ns-name 'minitest)
      (apply orig-load-libs args)
      (do (orig-load-libs '{minitest minitest} nil nil '[minitest] 'cljs.user)
          (let [emitted (with-out-str (apply orig-load-libs args))]
            (when (seq emitted)
              (binding-js [minitest/*currently-loading* true]
                (print emitted)))))))

  (defn- apply-patch-to-cljs-compiler-load-libs []
    (add-hook #'cljs.compiler/load-libs
              #'cljs-compiler-load-libs-around-hook))

  (defn- dequote [form]
    (when (and  (sequential? form)  (-> form first (= 'quote)))
      (second form)))

  (defn- minitest-req? [form]
    (or (= (dequote form) 'minitest)
        (and (sequential? form)
             (= (-> form first dequote) 'minitest))))

  (defn cljs-core-require-around-hook [orig-require &form &env & args]
    (if (or (-> &env :ns :name) (some minitest-req? args))
      (apply orig-require &form &env args)
      (apply orig-require &form &env (cons `'~'minitest args))))

  (defn- apply-patch-to-cljs-core-require []
    (add-hook #'cljs.core/require
              #'cljs-core-require-around-hook)))
