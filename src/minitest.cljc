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

(defn- load-tests? []
  #?(:clj  (and clojure.test/*load-tests* (-> (config) :load-tests))

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
   :load-tests  true
   :fail-fast   false
   :out         *out*
   :term-width  120
   :error-depth 12
   :silent      false
   :dots        false
   :langs       [:clj]
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

   :runner      {:class            Runner}
   :reporter    {:class            TermReporter}
   :executor    {:clj  {:class     CljExecutor}
                 :cljs {:class     CljsExecutor}}
   :DEFAULT-CTX {:exec-mode :on-eval
                 :env       :dev
                 :js-env    :node}
   :WHEN        (let [silent-success
                      {:WHEN {:status    {:success {:silent    true}}}}
                      run-on-load
                      {:WHEN {:exec-mode {:on-load {:run-tests true}}}}]
                  ;; reads as:
                  ;; when        =          then
                  {:exec-mode {:on-load     {:store-tests true
                                             :run-tests   false}
                               :on-eval     {:store-tests false
                                             :run-tests   true}}
                   :env       {:production  {:load-tests  false}
                               :cli         {:dots        true}
                               :ci          [:cli]
                               :dev         (merge run-on-load
                                                   {:break-on-failure true})
                               :quiet-dev   [:dev, silent-success]}
                   :status    {:success {:logo "✅"}
                               :failure {:logo "❌"}
                               :error   {:logo "🔥"}}})

   :break-on-failure false ;; TODO
   })

   (include "monkeypatch_load")
   (macros/deftime  (when (load-tests?) (apply-patches)))

       (newline)

