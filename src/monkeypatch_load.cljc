
(defn clear-tests!        [a & nss]    (swap! a #(apply dissoc % nss)))
(defn add-tests!          [a ns blocs] (swap! a update ns concat blocs))
(defn store-tests!        [ns blocs]   (add-tests! *tests*            ns blocs))
(defn process-after-load! [ns blocs]   (add-tests! *tests-to-process* ns blocs))

(defn process-tests-on-load-now! []
  (assert *currently-loading*)
  (try
    (let [conf (config)]
      (when (:store conf) (run! #(apply store-tests! %) @*tests-to-process*))
      (when (:run conf)   (run-execute-report! :suite @*tests-to-process*)))
    (finally
      (reset! *tests-to-process* nil))))

;; Clojure
(macros/deftime
  (defn- around-clj-load-hook [orig-load & paths]
    (println "LOADING" paths)
    (println "with repl env" (boolean cljs.repl/*repl-env*)
             "and depth" (count (seq (.getStackTrace (ex-info "X" {})))))
    ; (pprint (seq (.getStackTrace (ex-info "X" {}))))
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
              (when (or (:store conf) (:run conf))
                (process-tests-on-load-now!))
              load-result)))))

  (defn- apply-patch-to-clojure-core-load []
    (add-hook #'clojure.core/load
              #'around-clj-load-hook)))

;; ClojureScript
; (defn around-cljs-compiler-emit* [orig-emit & args]
;   (println "-------------- ORIG_EMIT" (type orig-emit))
;   (let [dispatch-val (-> args first :op)
;         method       (get-method orig-emit dispatch-val)]
;     (if (= dispatch-val :ns*)
;       (do (cljs.compiler/emitln "try {")
;           (apply method args)
;           (cljs.compiler/emitln))
;       (apply method args)))

;   (cljs.compiler/emitln)
;   (apply orig-emit args)
;   (cljs.compiler/emitln))

; (defn- apply-patch-to-cljs-compiler-emit* []
;   (add-hook #'cljs.compiler/emit*
;             #'around-cljs-compiler-emit*))
