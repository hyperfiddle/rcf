(ns minitest.base-config
  (:require [minitest.custom-map #?@(:clj  [:refer        [at-runtime]]
                                     :cljs [:refer-macros [at-runtime]])]))


(def base-config
  "Any config you may provide to minitest will merge into this base
  configuration map.

  See `(source base-config)`."
  {:dirs              ["src" "test"] ;; TODO: use clojure.java.classpath/classpath
   :elide-tests       false
   :out               *out*
   ;; Runner opts
   :fail-fast         false
   :break-on-failure  false ;; TODO
   ;; Reporter opts
   :term-width        120
   :error-depth       12
   :silent            false
   :dots              true
   :report            {:enabled     true
                       :level       :case}
   :explain           {:enabled     true
                       :level       :case
                       :WHEN {:dots {true {:WHEN {:status {:error   {:level :suite}
                                                           :failure {:level :suite}}}}}}}
   ;; Try this.
   ; :report            {:enabled     true
   ;                     :WHEN {:status {:success {:level :suite}
   ;                                     :failure {:level :ns}
   ;                                     :error   {:level :case}}}}
   ; :explain           {:enabled     true
   ;                     :level       :suite
   ;                     :WHEN {:status {:success {:level :suite}
   ;                                     :failure {:level :ns}
   ;                                     :error   {:level :case}}}}
   :stats             {:enabled     true
                       :level       :suite
                       :for         [:success :failure :error]}
   :effects           {:show-form   false
                       :show-result false
                       :silent      false}
   :announce-fn       (at-runtime (deref (resolve 'minitest.higher-order/do-nothing)))
   :announce-nb-tests true

   :bindings {:WHEN {:test-level
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
   :langs             [:clj]

   :run-fn            (at-runtime (deref (resolve 'minitest.runner/run)))
   :execute-fn        ::not-set! ;; Set in function of the :lang context.
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

   :CTX  {:exec-mode      :on-eval            ;; #{:on-eval :on-load}
          :env            :dev                ;; #{:production :lib :cli :ci
                                              ;;   :dev :quiet-dev}
          :js-env         :node               ;; TODO
          :last-in-level  :minitest/not-set!  ;; #{true false}
          :first-in-level :minitest/not-set!  ;; #{true false}
          :test-level     :minitest/not-set!  ;; #{:suite :ns :block :case}
          :test-position  :minitest/not-set!  ;; #{:before :after :do}
          :status         :minitest/not-set!  ;; #{:success :failure :error}
          :location       :minitest/not-set!  ;; #{:expectation :effect}
          :report-action  :minitest/not-set!  ;; #{:report :explain}
          :report-level   :case}              ;; #{:suite :ns :block :case}
   :WHEN (let [silent-success
               {:WHEN {:status    {:success {:silent    true}}}}
               run-on-load
               {:WHEN {:exec-mode {:on-load {:run-tests true}}}}]
           ;; reads as:
           ;; when       is    then         is
           {:lang       {:clj  {:execute-fn (at-runtime (deref (resolve 'minitest.executor/execute-clj)))}
                         :cljs {:execute-fn (at-runtime (deref (resolve 'minitest.executor/execute-cljs)))}}
            :exec-mode  {:on-load     {:store-tests    true
                                       :run-tests      false}
                         :on-eval     {:store-tests    false
                                       :run-tests      true}}
            :env        {:production  {:elide-tests    true}
                         :lib         {:elide-tests    false}
                         :cli         {:dots           true}
                         :ci          [:cli]
                         :dev         run-on-load
                         :quiet-dev   [:dev, silent-success]}
            :status     {:success     {:logo                  "✅"
                                       :WHEN {:location
                                              {:effect {:logo "[Effect] "}}}}
                         :failure     {:logo                  "❌"}
                         :error       {:logo                  "🔥"
                                       :WHEN {:location
                                              {:effect {:logo "😱[Effect] "}}}}}
            :test-level {:suite       {:announce-fn (at-runtime (deref (resolve 'minitest.reporter/announce-suite)))}
                         :ns          {:announce-fn (at-runtime (deref (resolve 'minitest.reporter/announce-ns)))
                                       :WHEN {:position {:after {:separator "\n"}}
                                              :dots {true  {:WHEN {:report-level {:suite {:WHEN {:position {:before {:announce-fn (fn [s p l n d]
                                                                                                                                    (println "-- Problems in" n))}
                                                                                                            :after  {:announce-fn (at-runtime (deref (resolve 'minitest.higher-order/do-nothing)))}}}}}}}}}}
                         :block       {:WHEN {:position {:after {:separator "\n"
                                                                 :WHEN {:dots {true {:WHEN {:report-level {:case {:separator "|"
                                                                                                                  :WHEN {:last-in-level {true {:separator "\n"}}}}}}}}}}}}}
                         ;; TODO: fix bug in minitest.reporter/separate-levels|
                         :case        {:WHEN {:position {:after {:WHEN {:dots {true {:WHEN {:report-level {:suite {:separator "\n"
                                                                                                                   :WHEN {:test-level {:case {:separator false}}}}}}}}}}}}}
                         :stats       {:WHEN {:position {:before {:separator "   "}
                                                         :after {:separator "\n"}}}}}})})
