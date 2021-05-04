(ns minitest.around-load
  (:require
    #?(:clj [cljs.analyzer.api                :as           ana])
    #?(:clj [cljs.compiler                    :as           cljsc])
    #?(:clj [cljs.repl])
    #?(:clj [robert.hooke                     :refer        [add-hook]])
            [net.cgrand.macrovich             :as           macros]
    #?(:clj [minitest.utils                   :refer        [->|]])
            [minitest.higher-order #?@(:clj  [:refer        [levels-above
                                                             do-nothing
                                                             if|]]
                                       :cljs [:refer        [levels-above
                                                             do-nothing]
                                              :refer-macros [if|]])]
            [minitest.dbg          #?@(:clj  [:refer        [dbg]]
                                       :cljs [:refer-macros [dbg]])]
            [minitest.orchestrator            :refer        [run-execute-report!]]
            [minitest.config       #?@(:clj  [:refer        [config
                                                             with-config
                                                             with-context]]
                                       :cljs [:refer        [config]
                                              :refer-macros [with-config
                                                             with-context]])])
  #?(:cljs
      (:require-macros
        [minitest.around-load                 :refer        [currently-loading?
                                                             tests-to-process]])))

(def ^:dynamic          *tests*             (atom nil))
(def ^:dynamic ^:no-doc *currently-loading* false)
(def ^:dynamic ^:no-doc *tested-ns*         nil)
(def ^:dynamic ^:no-doc *tests-to-process*  (atom nil))

(macros/deftime
  (defmacro currently-loading? []
    (macros/case
      :clj  `*currently-loading*
      :cljs `(when (cljs.core/exists? js/_MINITEST_CURRENTLY_LOADING_)
               js/_MINITEST_CURRENTLY_LOADING_)))

  (defmacro tests-to-process []
    (macros/case
      :clj  `*tests-to-process*
      :cljs `(when (cljs.core/exists? js/minitest._MINITEST_TESTS_TO_PROCESS_)
               js/minitest._MINITEST_TESTS_TO_PROCESS_))))

(defn clear-tests!   [a & nss]    (swap! a #(apply dissoc % nss)))
(defn add-tests!     [a ns blocs] (swap! a update ns concat blocs))
(defn store-tests!   [ns blocs]   (add-tests! *tests*            ns blocs))
(defn process-later! [ns blocs]   (add-tests! (tests-to-process) ns blocs))

(defn run-tests-at-level [l ns tests]
  (case l
    :suite (run-execute-report! l tests)
    :ns    (run-execute-report! l ns (select-keys tests [ns]))
    :block (run-execute-report! l ns (->> (get tests ns) (apply concat)))
    :case  (doall (map (partial run-execute-report! l ns)
                       (->> (get tests ns) (apply concat))))))

(defn process-now! [l & [ns]]
  (when-not (= l :suite)  (assert ns))
  (try (let [conf   (config)
             tests  @(tests-to-process)
             grrr!  run-execute-report!]
         (when (:store-tests conf)
           (run! #(apply store-tests! %) tests))
         (when (:run-tests conf)
           (run-tests-at-level l ns tests)))
    (finally
      (reset! (tests-to-process) nil))))

(defn ^:export store-and-run-tests-after-load! []
  (let [conf (config)]
    (when (and (or (:store-tests conf) (:run-tests conf))
               (if (currently-loading?)
                 (> (count @(tests-to-process)) 0)
                 true))
      (process-now! :suite))))

;; Clojure
(macros/deftime
  (def ^:dynamic ^:private *executing-cljs* false)

  (declare apply-cljs-patches apply-shadow-patches)

  (defn- clj-core-load-around-hook [orig-load & paths]
    (dbg "AROUND LOAD" paths (str "(for " (if *executing-cljs* :cljs :clj) ")"))
    (let [path->ns (->> (all-ns)
                        (mapv (juxt
                                (->| str @#'clojure.core/root-resource)
                                identity))
                        (into {}));; TODO: if the namespace isn't created ...
          ;; Assumption: load is always passed only one path.
          _        (assert (= 1 (count paths)))
          pth      (first paths)
          ns       (-> pth path->ns str symbol)]
      (when-not (#{"/cljs/core" "/cljs/compiler"} pth) (apply-cljs-patches))
      (when-not (#{"/shadow/build/compiler"} pth)      (apply-shadow-patches))
      ;; TODO: test whether requiring minitest before shadow works.

      (if (or cljs.repl/*repl-env* *executing-cljs*)
        (apply orig-load paths)
        (binding [*currently-loading* true
                  *tested-ns*         ns
                  *tests-to-process*  (atom nil)]
          (with-context {:exec-mode :on-load  :ns ns}
            (let [load-result (do (clear-tests! *tests* ns)
                                  (apply orig-load paths))]
              (store-and-run-tests-after-load!)
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
        (e "var _MINITEST_CURRENTLY_LOADING_;")
        (e "console.log('RAAAAAGE');")
        (e "const " orig-tests     " = minitest._MINITEST_TESTS_TO_PROCESS_")
        (e "const " orig-currently " = _MINITEST_CURRENTLY_LOADING_;")
        (e "minitest._MINITEST_TESTS_TO_PROCESS_ = cljs.core.atom.call(null,null);")
        (e "var _MINITEST_CURRENTLY_LOADING_ = true;")

        (e js)

        (e (cljsc/munge `store-and-run-tests-after-load!) ".call(null, null);")
        (e "minitest._MINITEST_TESTS_TO_PROCESS_  = " orig-tests     ";")
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
    (add-hook (resolve 'shadow.build.compiler/shadow-emit)
              #'shadow-build-compiler-shadow-emit-around-hook))

  ; (defn cljs-repl-node-node-eval-around-hook [orig-eval & [repl-env js
  ;                                                          :as args]]
  ;   (dbg "EVALUATING JS")
  ;   (dbg js)
  ;   (dbg "")
  ;   (apply orig-eval args))

  ; (defn- apply-patch-to-cljs-repl-node-node-eval []
  ;   (add-hook #'cljs.repl.node/node-eval
  ;             #'cljs-repl-node-node-eval-around-hook))


  (defn apply-clj-patches []
    (let [langs (-> (config) :langs set)]
      (when (or (:clj  langs) (:cljs langs))
        (apply-patch-to-clojure-core-load))))

  (defn apply-cljs-patches []
    (let [langs (-> (config) :langs set)]
      (when (:cljs langs)
        (when (find-ns 'cljs.core)      (apply-patch-to-cljs-core-require))
        (when (find-ns 'cljs.compiler)  (apply-patch-to-cljs-compiler-emit))

        ;; TODO: remove
        ; (when (find-ns 'cljs.repl.node) (apply-patch-to-cljs-repl-node-node-eval))
        )))

  (defn apply-shadow-patches []
    (let [langs (-> (config) :langs set)]
      (when (:cljs langs)
        (when (find-ns 'shadow.build.compiler)
          (apply-patch-to-shadow-build-compiler-shadow-emit))))))
