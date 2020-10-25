(ns minitest
  #?(:clj (:use clojure.pprint))
  #?(:clj (:gen-class))
  (:refer-clojure :exclude [test unquote])
  (:require [clojure.test]
   #?(:clj  [clojure.tools.namespace.find :refer [find-namespaces-in-dir]])
   #?(:clj  [clojure.java.io              :as    io])
            [clojure.edn                  :as    edn]
            [clojure.string               :as    str]
            [clojure.walk                 :refer [postwalk]]))

(declare tests test!)

(def ^:dynamic *tests*    (atom {}))
(def ^:dynamic *config*)
(def ^:dynamic *contexts* {:exec-mode :eval, :env :development})

(def ^:no-doc ^:dynamic *currently-loading* false)
(def ^:no-doc ^:dynamic *tests-to-process*  nil)

(declare config)
(def ^:no-doc ->|    #(apply comp (reverse %&)))
(def ^:no-doc call   #(apply %1 %&))

(defn- load-tests? []
 #?(:clj  (and clojure.test/*load-tests* (-> (config) :load-tests))
    :cljs true)) ;; will alway be run from a cljs REPL.

(load-file "src/config.cljc")
(load-file "src/runner.cljc")
(load-file "src/clojurescript.cljc")
(load-file "src/executor.cljc")
(load-file "src/reporter.cljc")
(load-file "src/run_execute_report.cljc")
(load-file "src/ns_selector.cljc")
(load-file "src/monkeypatch_load.cljc")

(def default-config
  "Any config you may provide to minitest will merge into this base
  configuration map.

  See `(source default-config)`."
  {:dirs         ["test"]
   :load-tests   true
   :runner       {:class            minitest.Runner
                  :fail-early       false
                  :break-on-failure false}
   :reporter     {:class            minitest.TermReporter
                  :term-width       80
                  :error-depth      20
                  :compact          true
                  :silent           false
                  :contexts {:status {:success {:logo '✅}
                                      :failure {:logo '❌}
                                      :error   {:logo '🔥}}}}
   :executor     {:clj  {:class     CljExecutor}
                  :cljs {:class     CljsExecutor
                         :cljsbuild {:source-paths [cljs-gen-src-path]
                                     :compiler {:output-to     cljs-gen-out-path
                                                :main          nil
                                                :optimizations :none}}}}
   :executors    [:clj :cljs]
   :contexts     {:exec-mode {:load          {:store true,  :run false}
                              :eval          {:store false, :run true}}
                  :env       {:production    {:load-tests                  false}
                              :development   {:runner   {:break-on-failure true}}
                              :cli           {:reporter {:dots             true}}
                              :ci            [:cli]}}})

#?(:clj (when (load-tests?)  (apply-patch-to-load)))

;; ## When and how to run tests
;; - [√] tests are run once
;; - [√] tests have absolutely no impact (i.e. the macro expands
;;       to nothing) when configured for a production environment.
;; - [√] tests are registered and/or run at load time or with eval according
;;       to the config.
;; - [√] tests are run once every var has loaded to avoid introduce code
;;       ordering problems to the already overwhelmed programmer.
;; - [√] `test!` can be used at load time to run the tests registered so far.
;; - [ ] works well with reload and var unloading (clojure.tools.namespace)
;; - [√] when tests are run via the clj test runner or explicitly in the repl
;;       with the test! fn:
;;       - successes: reported.
;;       - failures:  reported.
;; - [√] when tests are *implicitly* run from the repl:
;;       - successes: silenced.
;;       - failures: reported.

;; ## Test selectors
;; - The CLI runner should:
;;   - [√] run all tests if no args are provided
;;   - [√] otherwise proceed with a:
;;         - whitelist logic using one or more namespace selectors (the args):
;;           - [√] a ns name
;;           - [√] a ns glob ("my.ns.*")
;;           - [√] a regex
;;           - [√] a predicate fn
;;         - and a blacklist logic using these same selectors but:
;;           - [!] prefixed with "!" (ns name & ns globs only).
;;           - [ ] or by providing a sequence to an ":exclude" option
;; - The test! fn behaves the same but when no args are provided:
;;   - [ ] it runs tests for the local namespace if it possesses minitest tests.
;;   - [ ] it runs all the tests otherwise (for instance in the REPL from the
;;         "user" namespace).

;; ## Config
;; - Options:
;;   - [ ] :fail-early.
;;   - [ ] :break-on-failure (like https://github.com/ConradIrwin/pry-rescue).
;;   - [ ] :silent-success.
;; - [ ] a default config for each environment (CLI, REPL, on-load).
;; - [ ] which can be overriden in a project's minitest.edn file
;; - [ ] which can be overriden by ENV_VARS
;; - [ ] which can be overriden by args passed to the test! fn or the CLI Runner

;; ## Report format
;; - [ ] JUnit (a bit more work for a bit more readability in CIs, especially with
;;       lot of tests).
;; - [ ] std out is enough (not great with lot of tests)

(defn ^:no-doc juxtmap [& {:as m}]
  (fn [& args]
    (->> m
         (map #(as-> % [k v]  [k (apply v args)]))
         (into (empty m)))))

(def ^:no-doc as-thunk #(do `(fn [] ~%)))
(def ^:no-doc as-quote #(do `'~%))

(defmacro tests [& body]
  (when (load-tests?)
    (let [this-file     (io/file (ClassLoader/getSystemResource *file*))
          line-col-body (if this-file
                          (->> (-> &form meta (select-keys [:line :column]))
                               (read-forms-upto this-file)
                               last
                               rest) ;; drop initial 'tests symbol
                          body)
          parsed        (->> (partition 3 1 line-col-body)
                             (filter (->| second #{'=>}))
                             (map (juxt first last)))] ;; [test, expectation]
      `(let [c#     (config)
             ns#    (ns-name *ns*)
             block# ~(mapv (juxtmap :test
                                    (juxtmap :form  (->| first as-quote)
                                             :thunk (->| first as-thunk))
                                    :expectation
                                    (juxtmap :form  (->| second as-quote)
                                             :thunk (->| second as-thunk)))
                           parsed)]
         (if *currently-loading*
           (when (or (:store c#) (:run c#)) (process-after-load!    ns# [block#]))
           (do (when (:store c#)            (store-tests!           ns# [block#]))
               (when (:run   c#)            (run-execute-report!
                                              :block ns# block#))))
         nil))))

(defn- find-test-namespaces []
  (let [dirs (-> (config) :dirs set)]
    ;; TODO: won't look for cljs files
    (mapcat (->| io/file find-namespaces-in-dir)
            dirs)))

(defn- config-kw? [x]
  (and (keyword? x)
       (not (#{:exclude :all} x)))) ;; kws used for namespace selection

(defn- excludor
  "Works like clojure.core/or but returns false if one of the values
  appears to be :exclude. Not a macro, no control flow."
  [& [a & [b & more] :as all]]
  (cond
    (:exclude (set [a b])) false
    (seq more)            (apply excludor (or a b) more)
    :else                 (or a b)))

(defn test!
  ([]       (let [ns (ns-name *ns*)]
              (cond
                *currently-loading* (with-config {:run true :store false}
                                      (process-tests-on-load-now!))
                (get @*tests* ns)   (test! ns)
                :else               (test! :all))))
  ([& args] (let [[conf sels] (->> (partition-all 2 args)
                                   (split-with (->| first config-kw?)))
                  sels        (parse-selectors (apply concat sels))
                  nss         (-> (filter
                                    (->| (apply juxt sels)
                                         (partial apply excludor))
                                    (find-test-namespaces))
                                  (doto (->> (run! require))))
                  ns->tests   (select-keys @*tests* nss)]
              (if (empty? ns->tests)
                :no-test
                (with-config conf
                  (run-execute-report! :suite ns->tests)
                  nil)))))


;; TODO: move to test
;; (binding [... does not work since "tests" is a macro.
; (alter-var-root #'clojure.test/*load-tests* (constantly false))
; (try (tests :should-not-load => :should-not-load)
;   (finally (alter-var-root #'clojure.test/*load-tests* (constantly true))))

;; TODO:
;; - [√] tests should not run twice (when loaded, then when they are run)
;; - [ ] display usage
;; - [x] options are passed in a bash style (e.g. --option "value")
;; - [√] or options are passed clojure style (e.g. :option "value")

(defn -main [& args]
  (->> (str \[ (str/join \space args) \])
       edn/read-string
       (apply test!)))

;; TODO:
;; - [ ] a nice README.
;; - [ ] more private vars
