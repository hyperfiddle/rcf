(ns minitest
  (:gen-class)
  (:refer-clojure :exclude [test unquote])
  (:require [clojure.test]
            [clojure.java.io              :as io]
            [clojure.edn                  :as edn]
            [clojure.string               :as str]
            [clojure.tools.namespace.find :refer [find-namespaces-in-dir]]
            [clojure.walk                 :refer [postwalk]]))

(declare tests test!)
(def ^:dynamic *tests* (atom {}))
(def ^:dynamic *profile* nil)

(declare config)
(defn- load-tests? []
  (and clojure.test/*load-tests*
       (-> (config) :load-tests)))

(def ^:private ->|  #(apply comp (reverse %&)))
(def ^:private call #(apply %1 %&))

(load "reporter")
(load "ns_selector")
(load "runner")
(load "config")
(load "monkeypatch_load")


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
;; - [ ] when tests are run via the clj test runner or explicitly in the repl
;;       with the test! fn:
;;       - successes: reported.
;;       - failures:  reported.
;; - [ ] when tests are *implicitly* run from the repl:
;;       - successes: silenced.
;;       - failures: reported.

;; ## Test selectors
;; - The CLI runner should:
;;   - [ ] run all tests if no args are provided
;;   - [ ] otherwise proceed with a:
;;         - whitelist logic using one or more namespace selectors (the args):
;;           - [ ] a ns name
;;           - [ ] a ns glob ("my.ns.*")
;;           - [ ] a regex
;;           - [ ] a predicate fn
;;         - and a blacklist logic using these same selectors but:
;;           - [ ] prefixed with "!" (ns name & ns globs only).
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

(defn- construct-record [conf k assert-proto]
  (let [c (get-in conf [k :class])]
    (-> (.getCanonicalName c)
        ;; replace the last occurence of "." with "/map->" to find
        ;; the fully-qualified name of the constructor fn
        (str/replace #"\.(?=[^.]+$)" "/map->") ;; (?=...) is a regex lookahead
        (str/replace \_ \-) ;; TODO: other chars to replace ?
        symbol resolve deref
        (call {:opts  (get conf k)
               :store (atom {})}))))

(def ^:private runner   #(construct-record % :runner   RunnerP))
(def ^:private reporter #(construct-record % :reporter ReporterP))
(def ^:dynamic *runner*)
(def ^:dynamic *reporter*)

(defn- run-and-report! [x]
  (condp call x
    map?    (let [conf (config)]
              (binding [*runner*   (runner conf)
                        *reporter* (reporter conf)]
                (before-all    *reporter* x)
                (run-nss-tests *runner*   x)
                (after-all     *reporter* x)))
    vector? (let [[ns-name y] x]
              (condp call y
                  sequential? (do (before-ns    *reporter* ns-name y)
                                  (run-ns-tests *runner*   ns-name y)
                                  (after-ns     *reporter* ns-name y))
                  map?        (do (before-each  *reporter* ns-name y)
                                  (->> (run-one-test *runner*   ns-name y)
                                       (after-each   *reporter* ns-name)))))))


(defmacro ^:private ensuring-runner&reporter [& body]
  `(let [c# (config)]
     (binding [*runner*   (if-not (bound? #'*runner*)   (runner   c#) *runner*)
               *reporter* (if-not (bound? #'*reporter*) (reporter c#) *reporter*)]
       ~@body)))

(defn run-tests-now!             [ns cases] (ensuring-runner&reporter
                                              (run!
                                                #(run-and-report! [ns %])
                                                cases)))
(defn store-tests!               [ns cases] (swap! *tests* update ns
                                                   concat cases))
(defn process-tests-after-load!  [ns cases] (swap! *tests-to-process*
                                                   update ns concat cases))
(defn process-tests-on-load-now! [ns cases] (let [conf (config)]
                                              (when (-> conf :on-load :store)
                                                (run! #(apply store-tests! %)
                                                      @*tests-to-process*))
                                              (when (-> conf :on-load :run)
                                                (run! #(apply run-tests-now! %)
                                                      @*tests-to-process*))
                                              (reset! *tests-to-process* nil)))

(defn- juxtmap [m]
  (fn [& args]
    (->> m
         (map (fn [[k v]]
                [k (apply v args)]))
         (into (empty m)))))

(def ^:private as-thunk #(do `(fn [] ~%)))
(def ^:private as-quote #(do `'~%))

(defmacro tests [& body]
  (when (load-tests?)
    (let [parsed (->> (partition 3 1 body)
                      (filter (->| second #{'=>}))
                      (map (juxt first last)))] ;; [test, expectation]
      `(let [cases# ~(mapv
                       (juxtmap
                         {:test        (juxtmap {:form  (->| first as-quote)
                                                 :thunk (->| first as-thunk)})
                          :expectation (juxtmap {:form  (->| second as-quote)
                                                 :thunk (->| second as-thunk)})})
                       parsed)
             conf#  (config)
             ns#    (ns-name *ns*)]
         (if *currently-loading*
           (when (or (-> conf# :on-load :store)
                     (-> conf# :on-load :run))
             (process-tests-after-load! ns# cases#))
           (do (when (-> conf# :on-eval :store) (store-tests!   ns# cases#))
               (when (-> conf# :on-eval :run)   (run-tests-now! ns# cases#))))))))

(defn- find-test-namespaces []
  (let [dir  (-> (config) :dir)
        dirs (set (if (coll? dir) dir [dir]))]
    ;; TODO: won't look for cljs files
    (mapcat (->| io/file find-namespaces-in-dir)
            dirs)))

(defn- config-kw? [x]
  (and (keyword? x)
       (not (#{:exclude :all} x)))) ;; kws used for namespace selection

(defn test!
  ([]       (let [ns-nme (ns-name *ns*)]
              (cond *currently-loading*
                    ;; Force execution of tests not yet processed
                    (binding [*config* (config {:on-load {:run true}})]
                      (process-tests-on-load-now! ns-nme *tests-to-process*))

                    (contains? @*tests* ns-nme)
                    (test! ns-nme)

                    :else (test! :all))))
  ([& args] (let [[conf sels]  (->> (partition-all 2 args)
                                    (split-with (->| first config-kw?)))
                  sels         (parse-selectors (apply concat sels))
                  nss          (-> (find-test-namespaces)
                                   (filter (apply some-fn sels))
                                   (doto (->> (run! require))))
                  ns->tests    (select-keys @*tests* nss)]
              (binding [*config* (config conf)]
                (run-and-report! ns->tests)))))


;; TODO: move to test
;; (binding [... does not work since "tests" is a macro.
(alter-var-root #'clojure.test/*load-tests* (constantly false))
(try (tests :should-not-load => :should-not-load)
  (finally (alter-var-root #'clojure.test/*load-tests* (constantly true))))

(comment
  (defn test
    [options]
    (let [dir (or (:dir options) "test")
          dirs (if (coll? dir) dir [dir])
          nses (->> (set dirs)
                    (map io/file)
                    (mapcat find/find-namespaces-in-dir))
          nses (filter (ns-filter options) nses)]
      (println (format "\nRunning tests in %s" dirs))
      (dorun (map require nses))
      (try
        (filter-vars! nses (var-filter options))
        (apply test/run-tests nses)
        (finally
          (restore-vars! nses))))))

;; TODO:
;; - [√] tests should not run twice (when loaded, then when they are run)
;; - [ ] display usage
;; - [x] options are passed in a bash style (e.g. --option "value")
;; - [√] or options are passed clojure style (e.g. :option "value")
(def ^:private quote?            #(and (seq? %) (-> % first (= 'quote))))
(def ^:private unquote           second)
(def ^:private interpret-string  (->| edn/read-string
                                      #(if (symbol? %) resolve %)
                                      #(if (quote?  %) unquote %)))

(defn -main [& args]
  (apply test! (map interpret-string args)))

;; TODO:
;; - [ ] a nice README.
;; - [ ] more private vars
