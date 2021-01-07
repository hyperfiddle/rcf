
(defn clear-tests!        [a & nss]    (swap! a #(apply dissoc % nss)))
(defn add-tests!          [a ns blocs] (swap! a update ns concat blocs))
(defn store-tests!        [ns blocs]   (add-tests! *tests*            ns blocs))
(defn process-after-load! [ns blocs]   (add-tests! (tests-to-process) ns blocs))

(defn store-or-run-tests! []
  (assert (currently-loading?)) ;; TODO: remove ?
  (try
    (let [conf  (config)
          tests @(tests-to-process)]
      (when (:store-tests conf) (run! #(apply store-tests! %) tests))
      (when (:run-tests   conf) (run-execute-report! :suite   tests)))
    (finally
      (reset! (tests-to-process) nil))))

(defn ^:export store-or-run-tests-after-load! []
  (let [conf (config)]
    (when (and (or (:store-tests conf) (:run-tests conf))
               (if (currently-loading?)
                 (> (count @(tests-to-process)) 0)
                 true))
      (store-or-run-tests!))))

;; Clojure
(macros/deftime
  (defn- clj-core-load-around-hook [orig-load & paths]
    (dbg "*executing-cljs*" *executing-cljs*)
    (dbg "LOADING" paths)
    (if (or cljs.repl/*repl-env* *executing-cljs*)
      (apply orig-load paths)
      (let [path->ns (->> (all-ns)
                          (mapv (juxt
                                  (->| str @#'clojure.core/root-resource)
                                  identity))
                          (into {}))
            ;; Assumption: load is always passed only one path.
            ns       (-> paths first path->ns str symbol)]
        (binding [*currently-loading* true
                  *tested-ns*         ns
                  *tests-to-process*  (atom nil)]
          (with-context {:exec-mode :on-load  :ns ns}
            (let [load-result (do (clear-tests! *tests* ns)
                                  (apply orig-load paths))]
              (store-or-run-tests-after-load!)
              load-result))))))

  (defn- apply-patch-to-clojure-core-load []
    (add-hook #'clojure.core/load
              #'clj-core-load-around-hook)))

;; ClojureScript
(macros/deftime
  (defn- dequote [form]
    (if (and  (sequential? form)  (-> form first (= 'quote)))
      (second form)
      form))

    (defn- req-form? [lib form]
      (or (= (dequote form) lib)
          (and (sequential? form)
               (= (-> form first dequote) lib))))

  ;; TODO: keep ?
  (defn cljs-core-require-around-hook [orig-require &form &env & args]
    (if (or (= (ana/current-ns) 'minitest)
            (some #(req-form? 'minitest %) args))
      (apply orig-require &form &env args)
      (apply orig-require &form &env (cons `'~'minitest args)))) ;; TODO: ???

  (defn- apply-patch-to-cljs-core-require []
    (add-hook #'cljs.core/require
              #'cljs-core-require-around-hook))

  (defn handling-on-load-tests-in-js [js]
    (when (seq js)
      (let [e              cljsc/emitln
            orig-tests     (gensym "original_MINITEST_TESTS_TO_PROCESS_")
            orig-currently (gensym "original_MINITEST_CURRENTLY_LOADING_")]
        (e "goog.require('minitest');")
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
         ; (contains? (find-test-namespaces) name)
         (not= name 'minitest)))

  (defn cljs-compiler-emit-around-hook [orig-emit {:keys [op name] :as ast}]
    (when (-> ast :op (= :ns*))
      (dbg "\n")
      (dbg "grrrrrrrrrrrrrrrrrrrrr")
      (dbg "FORM:" (:form ast))
      (dbg "(ANA/CURRENT-NS)" (ana/current-ns))
      (dbg "AST NAME" (-> ast :name))
      (dbg "(-> ast :env :ns :name)" (-> ast :env :ns :name))
      (dbg "KEYS AST" (keys ast))
      (dbg "DEPS" (:deps ast))
      (dbg "REQ MINITEST ?" (some #(req-form? 'minitest %) (:deps ast)))
      (dbg "NAME" name)
      (dbg "\n"))
    (if (instrument-ast? :ns* ast)
      (do (handling-on-load-tests-in-js
            (with-out-str (orig-emit ast)))
          (when (-> ast :env :repl-env)
            (cljsc/emitln "'nil';")))
      (orig-emit ast)))

  (defn apply-patch-to-cljs-compiler-emit []
    (add-hook #'cljs.compiler/emit
              #'cljs-compiler-emit-around-hook))


  (defn- shadow-present? [] (find-ns 'shadow.build.cljs-hacks))

  (defn shadow-build-compiler-shadow-emit-around-hook [orig-shadow-emit
                                                       state {:keys [op name]
                                                              :as ast}]
    (when (-> ast :op (= :ns))
      (dbg "\n")
      (dbg "(ANA/CURRENT-NS)" (ana/current-ns))
      (dbg "AST NAME" (-> ast :name))
      (dbg "FORM" (-> ast :form)))
    (if (instrument-ast? :ns ast)
      (let [ast (assoc-in ast [:meta :skip-goog-provide] true)]
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
            (apply-patch-to-cljs-core-require)
            (apply-patch-to-cljs-compiler-emit)
            (apply-patch-to-cljs-repl-node-node-eval)
            (when (shadow-present?)
              (apply-patch-to-shadow-build-compiler-shadow-emit)))))))
