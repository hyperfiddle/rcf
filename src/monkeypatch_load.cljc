
(defn clear-tests!        [a & nss]    (swap! a #(apply dissoc % nss)))
(defn add-tests!          [a ns blocs] (swap! a update ns concat blocs))
(defn store-tests!        [ns blocs]   (add-tests! *tests*            ns blocs))
(defn process-after-load! [ns blocs]   (add-tests! (tests-to-process) ns blocs))

(defn store-or-run-tests! []
  (assert (currently-loading?)) ;; TODO: remove ?
  (try
    (let [conf (config)]
      (when (:store conf) (run! #(apply store-tests! %) @(tests-to-process)))
      (when (:run conf)   (run-execute-report! :suite   @(tests-to-process))))
    (finally
      (reset! (tests-to-process) nil))))

(defn store-or-run-tests-after-load! []
  (let [conf (config)]
    (when (and (or (:store conf) (:run conf))
               (if (currently-loading?)
                 (> (count @(tests-to-process)) 0)
                 true))
      (store-or-run-tests!))))

;; Clojure
(macros/deftime
  (defn- clj-core-load-around-hook [orig-load & paths]
    (dbg "LOADING" paths)
    (if cljs.repl/*repl-env*
      (apply orig-load paths)
      (with-contexts {:exec-mode :load}
          (binding [*currently-loading* true
                    *tests-to-process*  (atom nil)]
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
              (store-or-run-tests-after-load!)
              load-result)))))

  (defn- apply-patch-to-clojure-core-load []
    (add-hook #'clojure.core/load
              #'clj-core-load-around-hook)))

;; ClojureScript
(macros/deftime
  ;; TODO: keep ?
  (defn cljs-core-require-around-hook [orig-require &form &env & args]
    (if (-> &env :ns :name (= 'minitest))
      (apply orig-require &form &env args)
      (apply orig-require &form &env (cons `'~'minitest args))))

  (defn- apply-patch-to-cljs-core-require []
    (add-hook #'cljs.core/require
              #'cljs-core-require-around-hook))

  (defn handling-on-load-tests-in-js [js]
    (when (seq js)
      (let [e              cljsc/emitln
            orig-tests     (gensym "original_MINITEST_TESTS_TO_PROCESS_")
            orig-currently (gensym "original_MINITEST_CURRENTLY_LOADING_")]
        (e "var _MINITEST_TESTS_TO_PROCESS_;") ;; TODO: keep ?
        (e "var _MINITEST_CURRENTLY_LOADING_;")
        (e "const " orig-tests     " = _MINITEST_TESTS_TO_PROCESS_;")
        (e "const " orig-currently " = _MINITEST_CURRENTLY_LOADING_;")
        (e "var _MINITEST_TESTS_TO_PROCESS_ = cljs.core.atom.call(null,null);")
        (e "var _MINITEST_CURRENTLY_LOADING_ = true;")

        (e js)

        (e (cljsc/munge `store-or-run-tests-after-load!) ".call(null, null);")
        (e "var _MINITEST_TESTS_TO_PROCESS_  = " orig-tests     ";")
        (e "var _MINITEST_CURRENTLY_LOADING_ = " orig-currently ";"))))

  (defn instrument-ast? [target-op {:keys [op name] :as _ast}]
    (and (= op target-op)
         (contains? (find-test-namespaces) name)
         (not= name 'minitest)))


  (defn cljs-compiler-emit-around-hook [orig-emit {:keys [op name] :as ast}]
    ; (when (-> ast :op (= :ns*))
    ;   (dbg "\n")
    ;   (dbg "FUCK: cljs-compiler-emit-around-hook")
    ;   (dbg "(ANA/CURRENT-NS)" (ana/current-ns))
    ;   (dbg "AST NAME" (-> ast :name)))
    (if (instrument-ast? :ns* ast)
      (do (handling-on-load-tests-in-js
            (with-out-str (orig-emit ast)))
          (when (-> ast :env :repl-env)
            (cljsc/emitln "'nil';")))
      (orig-emit ast)))

  (defn apply-patch-to-cljs-compiler-emit []
    (add-hook #'cljs.compiler/emit
              #'cljs-compiler-emit-around-hook))



  (defn- dep-present? [dep] (re-find (re-pattern (str "/" dep "/[^/]+/" dep))
                                     (System/getProperty "java.class.path")))
  (defn- shadow-present? [] (dep-present? "shadow-cljs"))

  (defn shadow-build-compiler-shadow-emit-around-hook [orig-shadow-emit
                                                       state {:keys [op name]
                                                              :as ast}]
    (when (-> ast :op (= :ns))
      (binding [*out* (io/writer java.lang.System/out)]
        (newline)
        (println "(ANA/CURRENT-NS)" (ana/current-ns))
        (println "AST NAME" (-> ast :name))
        (println "FORM" (-> ast :form))))
    (if (instrument-ast? :ns ast)
      (let [_   (dbg "OKKKKK")
            ast (assoc-in ast [:meta :skip-goog-provide] true)]
        (cljsc/emitln "goog.provide('" (cljsc/munge name) "');")
        (handling-on-load-tests-in-js
          (with-out-str (orig-shadow-emit state ast))))
      (orig-shadow-emit state ast)))

  (defn apply-patch-to-shadow-build-compiler-shadow-emit []
    (require 'shadow.build.compiler)
    (add-hook (resolve 'shadow.build.compiler/shadow-emit)
              #'shadow-build-compiler-shadow-emit-around-hook))

  (defn cljs-repl-node-node-eval-around-hook [orig-eval & [repl-env js
                                                           :as args]]
    (dbg "EVALUATING JS")
    (dbg js)
    (dbg "")
    (apply orig-eval args))

  (defn- apply-patch-to-cljs-repl-node-node-eval []
    (add-hook #'cljs.repl.node/node-eval
              #'cljs-repl-node-node-eval-around-hook))


  (defn apply-patches []
    (let [langs (-> (config) :langs set)]
      (when (or (:clj  langs) (:cljs langs))
        (apply-patch-to-clojure-core-load))
      (when (:cljs langs)
        (do (apply-patch-to-clojure-core-load)
            ; (apply-patch-to-cljs-compiler-load-libs)
            ; (apply-patch-to-cljs-core-require)
            (apply-patch-to-cljs-compiler-emit)
            (apply-patch-to-cljs-repl-node-node-eval)
            (when (shadow-present?)
              (apply-patch-to-shadow-build-compiler-shadow-emit)))))))
