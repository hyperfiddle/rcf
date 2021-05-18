(ns minitest
  #?(:clj (:gen-class))
  (:require [net.cgrand.macrovich              :as           macros]
            [clojure.string                    :as           str]
            [clojure.pprint                    :refer        [pprint]]
   #?(:clj  [clojure.tools.namespace.repl      :refer        [disable-reload!]])
   #?(:clj  [clojure.repl                      :refer        [source-fn]])
   #?(:clj  [clojure.edn                       :as           edn])
            [minitest.ns                       :as           ns
                             #?@(:cljs [       :include-macros true])]
            [minitest.base-config              :refer        [base-config]]
            [minitest.executor                 :refer        [execute-clj
                                                              execute-cljs]]
            [minitest.reporter                 :refer        [report]]
            [minitest.orchestrator             :refer        [orchestrate
                                                              run-execute-report!]]
            [minitest.configuration            :as           cfg
                             #?@(:cljs [       :include-macros true])]
            [minitest.utils  #?@(:clj  [       :refer        [as-form
                                                              as-thunk
                                                              as-wildcard-thunk
                                                              ->|
                                                              meta-macroexpand
                                                              current-ns-name
                                                              defalias]]
                                 :cljs [       :refer        [as-form
                                                              as-thunk
                                                              as-wildcard-thunk
                                                              ->|]
                                               :refer-macros [current-ns-name
                                                               defalias]])]
            [minitest.around-load   #?@(:clj  [:refer        [apply-clj-patches
                                                              process-now!
                                                              process-later!
                                                              *tests*
                                                              currently-loading?]]
                                        :cljs [:refer        [process-now!
                                                              process-later!
                                                              *tests*]
                                               :refer-macros [currently-loading?]])]
            [minitest.unify                    :as           u
                                    #?@(:cljs [:include-macros true])]
            [minitest.tests-block              :as           tst
                                    #?@(:cljs [:include-macros true])]
            [minitest.custom-map               :as           cm]
            [minitest.higher-order             :as           higher])
  #?(:cljs
      (:require-macros [minitest.tests-block :as tst])))

(macros/deftime (disable-reload!))
(macros/deftime (macros/case :clj (apply-clj-patches)))

(defalias config         cfg/config)
(defalias ns-config      cfg/ns-config)
(defalias context        cfg/context)
(defalias config!        cfg/config!)
(defalias ns-config!     cfg/ns-config!)
(defalias context!       cfg/context!)

(macros/deftime
  (defalias with-config  cfg/with-config)
  (defalias with-context cfg/with-context)
  (defalias unified?     u/unified?)
  (defalias tests        tst/tests)
  (defalias at-runtime   cm/at-runtime)
  (defalias minifn       higher/minifn))

;; -- Dev tools
;; ---- Some commands
;; fswatch src/!(minitest.cljc) | (while read; do touch src/minitest.cljc; done)
;;
;; (cljs/build "test" {:main 'minitest-test :output-to "compiled.js" :output-dir "out" :optimizations :simple :target :nodejs})
;;
;; (require 'shadow.cljs.devtools.server) (shadow.cljs.devtools.server/start!) (require '[shadow.cljs.devtools.api :as shadow]) (shadow/watch :browser-support)
;;
;; (require '[cljs.repl :as repl]) (require '[cljs.repl.browser :as browser]) (repl/repl (browser/repl-env))


;; -- Explorations
; TODO: disable warnings with cljs.analyzer.api/no-warn
; See:  https://github.com/clojure/clojurescript/blob/5e88d3383e0f950c4de410d3d6ee11769f3714f4/src/main/clojure/cljs/analyzer/api.cljc#L140

;; TODO: *load-test*
;; See:  https://github.com/clojure/clojurescript/blob/5e88d3383e0f950c4de410d3d6ee11769f3714f4/src/main/clojure/cljs/analyzer.cljc#L61



;; DONE
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
;; - [√] effects can be run by putting '!!' before an expr, evaluating it
;;       without testing the result.

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
;;           - [x] prefixed with "!" (ns name & ns globs only).
;;                 Clashes with bash special chars.
;;           - [√] or by providing a sequence to an ":exclude" option
;; - The test! fn behaves the same but when no args are provided:
;;   - [√] it runs tests for the local namespace if it possesses minitest tests.
;;   - [√] it runs all the tests otherwise (for instance in the REPL from the
;;         "user" namespace).

;; ## Config
;; - Options:
;;   - [√] :fail-fast.
;;   - [ ] :break-on-failure (like https://github.com/ConradIrwin/pry-rescue).
;;   - [√] :silent-success.
;; - [√] a default config for each environment (CLI, REPL, on-load).
;; - [√] which can be overriden in a project's minitest.edn file
;; - [x] which can be overriden by ENV_VARS
;; - [√] which can be overriden by args passed to the test! fn or the CLI Runner

;; ## Report format
;; - [ ] JUnit (a bit more work for a bit more readability in CIs, especially with
;;       lot of tests).
;; - [√] configurable test output


;; ## More
;; - [x] NO. Warning on {:exec-mode {:on-eval {:store-tests true}}}
;;       (stores duplicate tests)
;; - [ ] Check exec mode of CLI runner
;; - [√] Clarify config names
;; - [ ] Config map init-fn
;; - [√] set-context!
;; - [√] namespaces as context

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
  ([]       (let [ns (current-ns-name)]
              (cond
                (currently-loading?) (cfg/with-config {:run-tests   true
                                                          :store-tests false}
                                       (process-now! :suite))
                (get @*tests* ns)    (test! ns)
                :else                (test! :all))))
  ([& args] (let [[conf sels]        (->> (partition-all 2 args)
                                          (split-with (->| first config-kw?)))
                  sels               (ns/parse-selectors (apply concat sels))
                  nss                (filter
                                       (->| (apply juxt sels)
                                            (partial apply excludor))
                                       (ns/find-test-namespaces))
                  ns->tests          (select-keys @*tests* nss)]
              (macros/case :clj (run! require nss))
              (if (empty? ns->tests)
                :no-test
                (cfg/with-config (into {} (map vec conf))
                  (run-execute-report! :suite ns->tests)
                  nil)))))

;; DONE:
;; - [√] tests should not run twice (when loaded, then when they are run)
;; - [√] display usage
;; - [x] options are passed in a bash style (e.g. --option "value")
;; - [√] or options are passed clojure style (e.g. :option "value")

;; TODO: deftime ?
(macros/case
  :clj
  (defn- print-usage []
    (println "Usage:" "clj -m minitest [config options] [ns selectors]")
    (println "with")
    (newline)
    (println "• config options: A flat config map of edn values that will")
    (println "                  get deep merged into minitest's existing")
    (println "                  config. Symbols will be resolved. Optional.")
    (println "• ns selectors:   One or more ns selectors:")
    (println "                    - a ns name.")
    (println "                    - a glob pattern to match namespaces.")
    (println "                    - the ':all' keyword.")
    (println "                    - ':exclude' followed by a ns selector.")
    (println "                    - a vector of ns selectors.")
    (println "                  Optional. Runs all the tests by default.")
    (println "                  If no selectors other than exclusive")
    (println "                  ones are specified, all the tests will be")
    (println "                  considered.")
    (newline)
    (println "Examples:")
    (println "  clj -m minitest name.space")
    (println "  clj -m minitest [name.space name.space.impl]")
    (println "  clj -m minitest name.*")
    (println "  clj -m minitest \\")
    (println "    {:WHEN {:status {:error {:logo \"🤯\"}}}} \\")
    (println "    hardship.impl")
    (newline)
    (println (source-fn `base-config))
    (newline)
    (let [confile (cfg/config-file)]
      (if confile
        (do (println "On top of this, here is your config from"
                     (str (->> confile .toURI
                               (.relativize (-> (java.io.File. ".") .toURI))
                               (str "./"))
                          ":"))
            (println (str/trim (slurp confile))))
        (do (println "On top of this, you have no config in either")
            (println "./minitest.edn or ./resources/minitest.edn"))))
    (newline)
    (println "And the resulting config after minitest merges them is:")
    (pprint (cfg/config))))

(macros/case
  :clj
  (defn -main [& args]
    (if (-> args first #{"help" ":help" "h" "-h" "--help"})
      (print-usage)
      (cfg/with-context {:env :cli}
        (->> (str \[ (str/join \space args) \])
             edn/read-string
             (apply test!))))))

;; TODO:
;; - [ ] a nice README.
