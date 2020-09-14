(ns minitest
  (:gen-class)
  (:refer-clojure :exclude [test])
  (:require [taoensso.timbre :refer [error]]
            [clojure.test]))

;; ## When and How to run tests
;;
;; For now, tests are run multiple times because the "tests" macro registers
;; and also runs the tests.
;; What I suggest or understood from your requirements:
;; - [√] tests have absolutely no impact in production (i.e. the macro expands
;;       to nothing).
;; - [ ] when tests are run via the clj test runner or explicitly in the repl
;;       with the test! fn:
;;       - successes: reported.
;;       - failures:  reported.
;; - [ ] when tests are *implicitly* run from the repl:
;;       - successes: silenced.
;;       - failures: reported.

;; ## Implicitly run tests
;; In development (.i.e not in production), tests are run implicitly:
;; - [ ] unless it is disabled by a flag.
;; - [ ] when the first "tests" block in a ns is executed, tests for this ns
;;      are cleared.
;; - [ ] when the last "tests" block in a ns is executed, it runs the tests for
;;       this ns.

(declare tests test!)
(def ^:dynamic *tests* (atom {}))

(def ^:private ->|      #(apply comp (reverse %&)))
(def ^:private as-thunk #(do `(fn [] ~%)))
(def ^:private as-quote #(do `'~%))

(defmacro tests [& body]
  (when (and clojure.test/*load-tests*
             ;; - [ ] Do you want a minitest specific flag for running tests ?
             ;;       https://github.com/marick/Midje/wiki/Production-mode
             )
    (let [parsed (->> (partition 3 1 body)
                      (filter (->| second #{'=>}))
                      (map (juxt first last))) ;; test & expectation
          cases  (->> parsed
                      (map (juxt (->| first  as-quote) (->| first  as-thunk)
                                 (->| second as-quote) (->| second as-thunk))))]
      `(do
         (swap! *tests* update (ns-name *ns*) concat ~(vec cases))
         ; if repl mode, just run them
         (test!)))))                                             ; danger

(defn- run-test! [[test-form test-thunk expect-form expect-thunk]]
  (let [test-v   (test-thunk)
        _        (do (set! *3 *2) (set! *2 *1) (set! *1 test-v))
        expect-v (expect-thunk)]
    (if (= test-v expect-v)
      (println "test passed" test-form "=>" expect-v)
      (error   "test failed" test-form "=>" expect-v expect-form))))

(defn test!
  ([] (test! (ns-name *ns*)))
  ([ns]
   (run! run-test! (get @*tests* (ns-name ns)))))

(tests
  Bla bla bla
  (inc 42) => 43
  (inc 3) => 4
  (inc 3) => 5
  nil => nil
  nil => true)

;; Tests can refer to the lexical environment
(let [a 1]
  (tests a => 1))

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
;; - [ ] tests should not run twice (when loaded, then when they are run)
(defn -main [& _args]
  (doseq [ns (keys @*tests*)]
    (test! ns)))
