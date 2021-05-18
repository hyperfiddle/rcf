(ns minitest.base-config
  (:require [minitest.custom-map #?@(:clj  [:refer        [at-runtime]]
                                     :cljs [:refer-macros [at-runtime]])]
            [minitest.utils                 :refer        [call
                                                           emphasize]]))


(def base-config
  "Any config you may provide to minitest will merge into this base
  configuration map.

  See `(source base-config)`."
  {:dirs              ["src" "test"] ;; TODO: use clojure.java.classpath/classpath
   :elide-tests       false
   :print-to          #?(:clj *out* :cljs cljs.core/*print-fn*)
   ;; Runner opts
   :fail-fast         false
   :break-on-failure  false ;; TODO
   ;; Reporter opts
   :term-width        120
   :error-depth       12
   :silent            false
   :dots              false

   :actions {:order   [:output :report :explain]
             :output  {:enabled true
                       :level   :case
                       :fn      (at-runtime (deref (resolve 'minitest.reporter/output-case)))}
             :report  {:enabled true
                       :level   :case
                       :fn      (at-runtime (deref (resolve 'minitest.reporter/report-case)))}
             :explain {:enabled true
                       :level   :case
                       :fn      (at-runtime (deref (resolve 'minitest.reporter/explain-case)))
                       :WHEN {:dots {true {:WHEN {:status {:error   {:level :suite}
                                                           :failure {:level :suite}}}}}}}}
   ;; Try this.
   ; :actions {:report            {:enabled     true
   ;                               :WHEN {:status {:success {:level :suite}
   ;                                               :failure {:level :ns}
   ;                                               :error   {:level :case}}}}
   ;           :explain           {:enabled     true
   ;                               :level       :suite
   ;                               :WHEN {:status {:success {:level :suite}
   ;                                               :failure {:level :ns}
   ;                                               :error   {:level :case}}}}}
   :stats             {:enabled     true
                       :level       :suite
                       :for         [:success :failure :error]}
   :effects           {:show-form   false
                       :show-result false
                       :silent      false}
   :announce-fn       (at-runtime (deref (resolve 'minitest.higher-order/do-nothing)))
   :announce-nb-tests false ;; TODO

   :bindings {:WHEN {:level
                     {:suite {:WHEN {:lang {:clj {#'*e (at-runtime (or *e nil))
                                                  #'*1 (at-runtime (or *1 nil))
                                                  #'*2 (at-runtime (or *2 nil))
                                                  #'*3 (at-runtime (or *3 nil))
                                                  }}}}
                      :block {:WHEN {:lang {:clj {#'*e (at-runtime (or *e nil))
                                                  #'*1 (at-runtime (or *1 nil))
                                                  #'*2 (at-runtime (or *2 nil))
                                                  #'*3 (at-runtime (or *3 nil))
                                                  }}}}}}}
   ;; Executor opts
   :langs             [:clj :cljs]

   :run-fn            (at-runtime (deref (resolve 'minitest.runner/run)))
   :execute-fn        (at-runtime (deref (resolve 'minitest.executor/execute)))
   :report-fn         (at-runtime (deref (resolve 'minitest.reporter/report)))
   :orchestrate-fn    (at-runtime (deref (resolve 'minitest.orchestrator/orchestrate)))


   :cljsbuild           {} ;; TODO: not in use
   :prepl-fn-sym        'cljs.server.node/prepl ;; TODO
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

   :CTX  {:exec-mode      :on-eval    ;; #{:on-eval :on-load}
          :env            :dev        ;; #{:production :lib :cli :ci
                                      ;;   :dev :quiet-dev}
          :js-env         :node       ;; TODO
          :last-in-level  nil         ;; #{true false}
          :first-in-level nil         ;; #{true false}
          :level          nil         ;; #{:suite :ns :block :case}
          :position       nil         ;; #{:before :after :do}
          :type           nil         ;; #{:expectation :effect}
          :status         nil         ;; #{:success :failure :error}
          :report-action  nil         ;; #{:output :report :explain}
          :report-level   :case}      ;; #{:suite :ns :block :case}
   :WHEN (let [silent-success
               {:WHEN {:status    {:success {:silent    true}}}}
               run-on-load
               {:WHEN {:exec-mode {:on-load {:run-tests true}}}}]
           ;; reads as:
           ;; when       is    then         is
           {:exec-mode  {:on-load     {:store-tests    true
                                       :run-tests      false}
                         :on-eval     {:store-tests    false
                                       :run-tests      true}
                         :inner-test  {:store-tests    false
                                       :run-tests      true}}
            :env        {:production  {:elide-tests    true}
                         :lib         {:elide-tests    false}
                         :cli         {:dots           true}
                         :ci          [:cli]
                         :dev         run-on-load
                         :quiet-dev   [:dev, silent-success]}
            :status     {:success     {:logo                            "✅"
                                       :WHEN {:type {:effect {:logo (emphasize "[Effect] ")}}}}
                         :failure     {:logo                            "❌"}
                         :error       {:logo                            "🔥"
                                       :WHEN {:type {:effect {:logo (emphasize "😱 [Effect] ")}}}}}
            :level      {:suite       {:announce-fn (at-runtime (deref (resolve 'minitest.reporter/announce-suite)))}
                         :ns          {:announce-fn (at-runtime (deref (resolve 'minitest.reporter/announce-ns)))
                                       :WHEN {:position {:after {:separator "\n"}}
                                              :dots {true  {:WHEN {:report-level {:suite {:WHEN {:position {:before {:announce-fn (fn [s p l n d]
                                                                                                                                    (println "-- Problems in" n))}
                                                                                                            :after  {:announce-fn (at-runtime (deref (resolve 'minitest.higher-order/do-nothing)))}}}}}}}}}}
                         :block       {:WHEN {:position {:after {:separator "\n"
                                                                 :WHEN {:dots {true {:WHEN {:report-level {:case {:separator "|"
                                                                                                                  :WHEN {:last-in-level {true {:separator "\n"}}}}}}}}}}}}}
                         ;; TODO: fix bug in minitest.reporter/separate-levels|
                         ; :case        {:WHEN {:position {:after {:WHEN {:dots {true {:WHEN {:report-level {:suite {:separator "\n"
                         ;                                                                                           :WHEN {:level {:case {:separator false}}}}}}}}}}}}}
                         :stats       {:WHEN {:position {:before {:separator "   "}
                                                         :after {:separator "\n"}}}}}})})
