(ns minitest
  (:gen-class)
  (:refer-clojure :exclude [test])
  (:require [clojure.test]
            [robert.hooke    :refer [add-hook]]
            [clojure.java.io :as io]
            [clojure.edn     :as edn]
            [clojure.string  :as str]))

;; ## When and how to run tests
;; - [√] tests are run once
;; - [√] tests have absolutely no impact (i.e. the macro expands
;;       to nothing) when configured for a production environment.
;; - [√] tests are registered and/or run at load time or with eval according
;;       to the config.
;; - [√] tests are run once every var has loaded to avoid introduce code
;;       ordering problems to the already overwhelmed programmer.
;; - [√] `test!` can be used at load time to run the tests registered so far.
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
;; - [ ] which can be overriden by args passed to the test! fn or the CLI runner

;; ## Report format
;; - [ ] JUnit (a bit more work for a bit more readability in CIs, especially with
;;       lot of tests).
;; - [ ] std out is enough (not great with lot of tests)

(declare tests test!)
(def ^:dynamic *tests* (atom {}))
(def ^:dynamic *profile* nil)

(def ^:private ->|      #(apply comp (reverse %&)))
(def ^:private as-thunk #(do `(fn [] ~%)))
(def ^:private as-quote #(do `'~%))

;; Taken from https://gist.github.com/danielpcox/c70a8aa2c36766200a95
(defn- deep-merge [& maps]
  (apply merge-with (fn [& args]
                      (if (every? #(or (map? %) (nil? %)) args)
                        (apply deep-merge args)
                        (last args)))
         maps))



(def default-config
  "Any config you may provide to minitest will merge into this base
  configuration map.

  See `(source default-config)`."
  {:fail-early       false
   :silent-success   false
   :break-on-failure false ; TODO
   :formatter        :simple
   :load-tests       true
   :on-load {:store  true
             :run    false}
   :on-eval {:store  false
             :run    true}
   :profiles {:production  {:load-tests       false}
              :development {:break-on-failure true}}})

(defn- read-config []
  (let [f (io/file "./minitest.edn")]
    (-> (or (when (.exists f) f)
            (io/resource "minitest.edn"))
        (some-> slurp edn/read-string))))

(def ^:dynamic *config* (read-config))

(defn- profile-config [profile]
  (let [profiles (:profiles *config*)
        raw-profile (get profiles profile)]
    (->> (if (sequential? raw-profile)
           raw-profile
           [raw-profile])
         (map #(cond (map? %)        %
                     (sequential? %) (profile-config %)
                     :else           (get profiles %)))
         (apply deep-merge))))

(defn config
  ([] (config *profile*))
  ([profile]
   (deep-merge default-config
               (dissoc *config* :profiles)
               (profile-config profile))))

(defn- clear-tests [ns-name]
  (swap! *tests* dissoc ns-name))

(defn- load-tests? []
  (and clojure.test/*load-tests*
       (-> (config) :load-tests)))

(defn ^:no-doc run-test! [[test-form test-thunk expect-form expect-thunk]]
  (let [test-v   (test-thunk)
        _        (do (set! *3 *2) (set! *2 *1) (set! *1 test-v))
        expect-v (expect-thunk)]
    (if (= test-v expect-v)
      (println "test passed" test-form "=>" expect-v)
      (println "test failed" test-form "=>" expect-v))))

(def ^:no-doc ^:dynamic *currently-loading* false)
(def ^:no-doc ^:dynamic *tests-to-process*  nil)

(defn run-tests-now!       [cases]    (run! run-test! cases))
(defn store-tests!         [ns cases] (swap! *tests* update ns concat cases))
(defn run-tests-gradually! [ns cases] (swap! *tests-to-process*
                                             update ns run-tests-now!))
(defn process-after-load!  [ns cases] (swap! *tests-to-process*
                                            update ns concat cases))

(defn hook-around-load [orig-load & paths]
  (binding [*currently-loading* true
            *tests-to-process* (atom nil)]
    (let [nss    (->> paths (map #(-> %
                                      (str/replace #"^/" "")
                                      (str/replace "/" ".")
                                      symbol)))
          result (do (run! clear-tests nss)
                     (apply orig-load paths))
          conf   (config)]
      (when (-> conf :on-load :store)
        (run! #(apply store-tests! %) @*tests-to-process*))
      (when (-> conf :on-load :run)
        (run! #(apply run-tests-now! %) @*tests-to-process*))
      result)))

(when (load-tests?)
  (add-hook #'clojure.core/load #'hook-around-load))

(defmacro tests [& body]
  (when (load-tests?)
    (let [parsed (->> (partition 3 1 body)
                      (filter (->| second #{'=>}))
                      (map (juxt first last)))] ;; test & expectation
      `(let [cases# ~(mapv (juxt (->| first  as-quote) (->| first  as-thunk)
                                 (->| second as-quote) (->| second as-thunk))
                           parsed)
             conf#  (config)
             mode#  (if *currently-loading* :on-load :on-eval)
             ns#    (ns-name *ns*)]
         (do (when (-> conf# mode# :store)
               (if *currently-loading*
                 (process-after-load! ns# cases#)
                 (store-tests! ns# cases#)))
             (when (-> conf# mode# :run)
               (if *currently-loading*
                 ;; Run the tests after vars have been loaded to avoid code
                 ;; ordering issues. So store the tests unless it has been done.
                 (when-not (-> conf# mode# :store)
                   (process-after-load! ns# cases#))
                 (run-tests-now! cases#))))))))

(defn test!
  ([] (test! (ns-name *ns*)))
  ([ns]
   (if *currently-loading*
     (run-tests-gradually! ns *tests-to-process*)
     (run-tests-now! (get @*tests* ns)))))

;; (binding [... does not work since "tests" is a macro.
(alter-var-root #'clojure.test/*load-tests* (constantly false))
(try (tests :should-not-load => :should-not-load)
  (finally (alter-var-root #'clojure.test/*load-tests* (constantly true))))

(comment
  ;; These are Koacha's runner CLI options, just as a source of inspiration
  (def ^:private cli-options
    [["-c" "--config-file FILE"   "Config file to read."
      :default "tests.edn"]
     [nil  "--print-config"       "Print out the fully merged and normalized config, then exit."]
     [nil  "--print-test-plan"    "Load tests, build up a test plan, then print out the test plan and exit."]
     [nil  "--print-result"       "Print the test result map as returned by the Kaocha API."]
     [nil  "--fail-fast"          "Stop testing after the first failure."]
     [nil  "--[no-]color"         "Enable/disable ANSI color codes in output. Defaults to true."]
     [nil  "--[no-]watch"         "Watch filesystem for changes and re-run tests."]
     [nil  "--reporter SYMBOL"    "Change the test reporter, can be specified multiple times."
      :parse-fn (fn [s]
                  (let [sym (symbol s)]
                    (if (qualified-symbol? sym)
                      sym
                      (symbol "kaocha.report" s))))
      :assoc-fn accumulate]
     [nil "--plugin KEYWORD"      "Load the given plugin."
      :parse-fn (fn [s]
                  (let [kw (parse-kw s)]
                    (if (qualified-keyword? kw)
                      kw
                      (keyword "kaocha.plugin" s))))
      :assoc-fn accumulate]
     [nil "--profile KEYWORD"     "Configuration profile. Defaults to :default or :ci."
      :parse-fn parse-kw]
     [nil "--version"             "Print version information and quit."]

     ;; Clojure CLI tools intercepts --help, so we add --test-help, but in other
     ;; circumstances it should still work.
     [nil "--help"                "Display this help message."]
     ["-H" "--test-help"          "Display this help message."]])
  )

;; TODO:
;; - [√] tests should not run twice (when loaded, then when they are run)
;; - [ ] display usage
;; - [ ] options are passed in a bash style (e.g. --option "value")
;; - [ ] or options are passed clojure style (e.g. :option "value")
(defn -main [& _args]
  (doseq [ns (keys @*tests*)]
    (test! ns)))

;; TODO:
;; - [ ] a nice README.
;; - [ ] more private vars
