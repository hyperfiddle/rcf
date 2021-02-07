(ns minitest.base-config
  (:require [minitest.utils #?@(:clj  [:refer        [at-runtime]]
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
   :dots              false
   :report            {:enabled     true
                       :level       :block}
   :explanation       {:enabled     true
                       :level       :block}
   :stats             {:enabled     true
                       :level       :suite
                       :for         [:success :failure :error]}
   :effects           {:show-form   false
                       :show-result false
                       :silent      false}

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

   :CTX  {:exec-mode      :on-eval    ;; #{:on-eval :on-load}
          :env            :dev        ;; #{:production :lib :cli :ci :dev :quiet-dev}
          :js-env         :node       ;; TODO
          :last-in-level  ::not-set!  ;; #{true false}
          :first-in-level ::not-set!  ;; #{true false}
          :test-level     ::not-set!  ;; #{:suite :ns :block :case}
          :status         ::not-set!  ;; #{:success :failure :error}
          :refreshing     false       ;; #{true false}
          :location       ::not-set!} ;; #{:expectation :effect}
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
            :status     {:success     {:logo           "✅"
                                       :WHEN {:location
                                              {:effect {:logo "[Effect] "}}}}
                         :failure     {:logo           "❌"}
                         :error       {:logo           "🔥"
                                       :WHEN {:location
                                              {:effect {:logo "😱[Effect] "}}}}}
            :test-level {:ns          {:post-separator "\n"}
                         :block       {:post-separator "\n"}
                         :stats       {:separator      "  "
                                       :WHEN {:dots {true {:separator ""}}}
                                       :post-separator "\n"}}})})
