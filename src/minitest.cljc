(ns minitest
  #?(:clj (:gen-class))

  (:refer-clojure :exclude [test unquote]) ;; TODO: remove unquote

  (:require [clojure.test]
            [clojure.string                    :as    str]
            [clojure.walk                      :refer [postwalk]]
            [net.cgrand.macrovich              :as    macros]
   #?(:clj  [clojure.pprint                    :as    pp :refer [pprint]]
      :cljs [cljs.pprint                       :as    pp :refer [pprint]])
   #?(:clj  [clojure.core.async                :as    async])
   #?(:clj  [clojure.core.server])
   #?(:clj  [clojure.java.classpath            :as    cp])
   #?(:clj  [clojure.java.io                   :as    io])
   #?(:clj  [clojure.spec.alpha                :as    s])
   #?(:clj  [clojure.tools.macro               :refer [symbol-macrolet]])
   #?(:clj  [clojure.tools.namespace.find      :refer [find-namespaces-in-dir
                                                       find-sources-in-dir]])
   #?(:clj  [clojure.tools.namespace.file      :refer [read-file-ns-decl]])
   #?(:clj  [clojure.tools.namespace.parse     :refer [name-from-ns-decl]])
   #?(:clj  [clojure.tools.namespace.repl      :refer [disable-reload!]])
   #?(:clj  [clojure.tools.reader              :as    r]
      :cljs [cljs.tools.reader                 :as    r])
   #?(:clj  [clojure.tools.reader.reader-types :as    rt])
   #?(:clj  [clojure.repl                      :refer [source-fn pst]])
   #?(:clj  [cljs.repl                         :as    repl])
   #?(:cljs [cljs.repl                         :refer [pst]])
   #?(:clj  [cljs.repl.node                    :as    node])
   #?(:clj  [cljs.repl.browser                 :as    browser])
   #?(:clj  [cljs.compiler                     :as    cljsc])
   #?(:clj  [cljs.core                         :as    cljs])
   #?(:clj  [cljs.analyzer.api                 :as    ana])
   #?(:clj  [cljs.env                          :as    env])
   #?(:clj  [clojure.edn                       :as    edn])
   #?(:clj  [robert.hooke                      :refer [add-hook]]))

  #?(:cljs
      (:require-macros [minitest :refer [include
                                         file-config
                                         once once-else
                                         doseq-each-executor
                                         for-each-executor
                                         ensuring-runner+executors+reporter
                                         cljs-src-path
                                         cljs-out-path
                                         current-ns-name
                                         find-test-namespaces
                                         with-context
                                         with-config
                                         managing-exs
                                         lay
                                         currently-loading?
                                         tests-to-process
                                         handling-on-load-tests-in-js
                                         defaccessors]]
                       [clojure.tools.macro :refer [symbol-macrolet]]))

  #?(:clj
      (:import [java.io      PipedReader PipedWriter PushbackReader]
               [java.net     Socket]
               [clojure.lang LineNumberingPushbackReader])))

(macros/deftime (disable-reload!))

;; -- Dev tools
;; ---- Some commands
;; fswatch src/!(minitest.cljc) | (while read; do touch src/minitest.cljc; done)

;; (cljs/build "test" {:main 'minitest-test :output-to "compiled.js" :output-dir "out" :optimizations :simple :target :nodejs})

;; (require 'shadow.cljs.devtools.server) (shadow.cljs.devtools.server/start!) (require '[shadow.cljs.devtools.api :as shadow]) (shadow/watch :app)

;; ---- Debugging
(macros/deftime
  (def ^:private ^:dynamic *debug* false)

  (macros/case
    :clj (defmacro dbg [& args]
           (when *debug*
             `(binding [*out* (io/writer java.lang.System/out)]
                (println ~@args))))))

;; -- Explorations
; TODO: disable warnings with cljs.analyzer.api/no-warn
; See:  https://github.com/clojure/clojurescript/blob/5e88d3383e0f950c4de410d3d6ee11769f3714f4/src/main/clojure/cljs/analyzer/api.cljc#L140

;; TODO: *load-test*
;; See:  https://github.com/clojure/clojurescript/blob/5e88d3383e0f950c4de410d3d6ee11769f3714f4/src/main/clojure/cljs/analyzer.cljc#L61


(declare tests test!)

(def ^:dynamic          *tests*             (atom {}))
(def ^:dynamic ^:no-doc *currently-loading* false)
(def ^:dynamic ^:no-doc *tests-to-process*  nil)

(macros/deftime
  (defmacro tests-to-process []
    (macros/case
      :clj  `*tests-to-process*
      :cljs `(when (cljs.core/exists? js/_MINITEST_TESTS_TO_PROCESS_)
               js/_MINITEST_TESTS_TO_PROCESS_)))

  (defmacro currently-loading? []
    (macros/case
      :clj  `*currently-loading*
      :cljs `(when (cljs.core/exists? js/_MINITEST_CURRENTLY_LOADING_)
               js/_MINITEST_CURRENTLY_LOADING_))))

(declare config)
(def ^:no-doc ->|   #(apply comp (reverse %&)))
(def ^:no-doc call  #(apply %1 %&))

(def ^:no-doc as-thunk       #(do `(fn [] ~%)))
(def ^:no-doc as-form        #(do `'~%))

(macros/deftime
  (defmacro current-file []
    (-> *file* ClassLoader/getSystemResource .getPath))

  (defmacro current-ns-name []
    `'~(ns-name *ns*))

  ;; wow, this works. Really ?
  (defmacro include [path]
    (macros/case
      :clj  `(load ~path)
      :cljs (let [^String path (str (@#'clojure.core/root-directory
                                      (current-ns-name))
                                    \/ path)]
              (when @#'clojure.core/*loading-verbosely*
                (printf "(minitest/include \"%s\")\n" path)
                (flush))
              (@#'clojure.core/check-cyclic-dependency path)
              (when-not (= path (first @#'clojure.core/*pending-paths*))
                (binding
                  [clojure.core/*pending-paths*
                   (conj @#'clojure.core/*pending-paths* path)
                   *read-eval*
                   true]
                  (let [dir (-> (io/file (current-file))
                                .getParentFile)
                        f   (some #(when (.exists %) %)
                                  (for [ext ["cljs" "cljc"]]
                                    (io/file dir (str (str/join (rest path))
                                                      \. ext))))]
                    `(do ~@(->> (str  \[  (-> f slurp)  \]  )
                                (r/read-string
                                  {:read-cond :allow
                                   :features (macros/case
                                               :clj  #{:clj}
                                               :cljs #{:cljs})}))))))))))

(include "config")
(include "clojurescript")
(include "prepl")
(include "executor")
(include "runner")
(include "reporter")
(include "run_execute_report")
(include "ns_selector")

(def base-config
  "Any config you may provide to minitest will merge into this base
  configuration map.

  See `(source base-config)`."
  {:dirs        ["src" "test"] ;; TODO: use clojure.java.classpath/classpath
   :elide-tests false
   :fail-fast   false
   :out         *out*
   :term-width  120
   :error-depth 12
   :silent      false
   :dots        false
   :langs       [:clj]

   :runner      {:class            Runner}
   :reporter    {:class            TermReporter}
   :executor    {:clj  {:class     CljExecutor}
                 :cljs {:class     CljsExecutor}}

   :cljsbuild   {} ;; TODO: not in use
   :prepl-fn    'cljs.server.node/prepl
   #_(:cljs nil
            :clj  {:js-env :node
                   ; :WHEN
                   ; {:js-env
                   ;  {:node          'cljs.server.node/prepl
                   ;   :browser       'cljs.server.browser/prepl
                   ;   ; :figwheel      'cljs.core.server/io-prepl
                   ;   ; :lein-figwheel 'cljs.core.server/io-prepl
                   ;   :rhino         'cljs.server.rhino/prepl
                   ;   :graaljs       'cljs.server.graaljs/prepl
                   ;   :nashorn       'cljs.server.nashorn/prepl}}
                   })

   :DEFAULT-CTX {:exec-mode :on-eval
                 :env       :dev
                 :js-env    :node}
   :WHEN        (let [silent-success
                      {:WHEN {:status    {:success {:silent    true}}}}
                      run-on-load
                      {:WHEN {:exec-mode {:on-load {:run-tests true}}}}]
                  ;; reads as:
                  ;; when        is          then
                  {:exec-mode {:on-load     {:store-tests true
                                             :run-tests   false}
                               :on-eval     {:store-tests false
                                             :run-tests   true}}
                   :env       {:production  {:elide-tests true}
                               :cli         {:dots        true}
                               :ci          [:cli]
                               :dev         run-on-load
                               :quiet-dev   [:dev, silent-success]}
                   :status    {:success     {:logo "✅"}
                               :failure     {:logo "❌"}
                               :error       {:logo "🔥"}}})

   :break-on-failure false ;; TODO
   })

   (include "monkeypatch_load")
   (macros/deftime  (when-not (-> (config) :elide-tests) (apply-patches)))

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
   ;; - [ ] namespaces as context

   (macros/deftime
     ;; TODO: too much
     (defn conform! [spec value]
       (let [result (s/conform spec value)]
         (when (= result :s/invalid)
           (throw (Exception.
                    (binding [*print-level* 7
                              *print-namespace-maps* false]
                      (str \newline
                           (with-out-str (->> (s/explain-data spec value)
                                              (mapv (fn [[k v]]
                                                      [(-> k name keyword) v]))
                                              (into {})
                                              pprint)))))))
         result))

     (defn- parse-tests [block-body]
       (let [not-op? (complement #{:= :?})]
         (->> block-body
              (conform!
                (s/*
                  (s/alt :effect  not-op?
                         :expectation (s/alt
                                        := (s/cat :tested   not-op?
                                                  :op       #{:=}
                                                  :expected not-op?)
                                        :? (s/cat :op       #{:?}
                                                  :tested not-op?)))))
              ;; Sample of what we are processing next:
              ;; [[:effect 0]
              ;;  [:expectation [:= {:tested 1, :op :=, :expected 1}]]]
              (mapv (fn [[type x]]
                      (case type
                        :effect
                        {:type     :effect
                         :form     (-> x as-form)
                         :thunk    (-> x as-thunk)}

                        :expectation
                        (let [[op m] x]
                          (merge {:type     :expectation
                                  :op       op
                                  :tested   {:form  (-> m :tested as-form)
                                             :thunk (-> m :tested as-thunk)}}
                                 (when (= op :=)
                                   {:expected {:form  (-> m :expected as-form)
                                               :thunk (-> m :expected as-thunk)}}))
                          )))))))

     (defmacro tests [& body]
       (when-not (-> (config) :elide-tests)
         `(with-context {:exec-mode (if (currently-loading?) :on-load :on-eval)}
            (let [c#     (config)
                  ns#    (current-ns-name)
                  block# ~(parse-tests body)]
              (if (currently-loading?)
                (when (or (:store-tests c#)
                          (:run-tests   c#)) (process-after-load! ns# [block#]))
                (when     (:run-tests   c#)  (run-execute-report! :block
                                                                  ns# block#))))
            nil))))

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
                   (currently-loading?) (with-config {:run-tests   true
                                                      :store-tests false}
                                          (store-or-run-tests!))
                   (get @*tests* ns)    (test! ns)
                   :else                (test! :all))))
     ([& args] (let [[conf sels]        (->> (partition-all 2 args)
                                             (split-with (->| first config-kw?)))
                     sels               (parse-selectors (apply concat sels))
                     nss                (filter
                                          (->| (apply juxt sels)
                                               (partial apply excludor))
                                          (find-test-namespaces))
                     ns->tests          (select-keys @*tests* nss)]
                 (macros/case :clj (doto (run! require nss)))
                 (if (empty? ns->tests)
                   :no-test
                   (with-config (into {} (map vec conf))
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
       (println (source-fn 'minitest/base-config))
       (newline)
       (let [confile (config-file)]
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
       (println "And the resulting config after minitest deep-merges them is:")
       (pprint (config))))

   (macros/case
     :clj
     (defn -main [& args]
       (if (-> args first #{"help" ":help" "h" "-h" "--help"})
         (print-usage)
         (with-context {:env :cli}
           (->> (str \[ (str/join \space args) \])
                edn/read-string
                (apply test!))))))

   ;; TODO:
   ;; - [ ] a nice README.
   ;; - [ ] more private vars
