(ns minitest.base-config)

(defn base-config []
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
   :report            {:enabled true
                       :level   :block}
   :explaination      {:enabled true
                       :level   :block}
   :stats             {:enabled true
                       :level   :suite
                       :for     [:success :failure :error]}
   :bindings {:WHEN {:test-level
                     {:suite {:WHEN {:lang {:clj {#'*e [:AT-RUNTIME #(or *e nil)]
                                                  #'*1 [:AT-RUNTIME #(or *1 nil)]
                                                  #'*2 [:AT-RUNTIME #(or *2 nil)]
                                                  #'*3 [:AT-RUNTIME #(or *3 nil)]}}}}
                      :block {:WHEN {:lang {:clj {#'*e [:AT-RUNTIME #(or *e nil)]
                                                  #'*1 [:AT-RUNTIME #(or *1 nil)]
                                                  #'*2 [:AT-RUNTIME #(or *2 nil)]
                                                  #'*3 [:AT-RUNTIME #(or *3 nil)]}}}}}}}
   ;; Executor opts
   :langs             [:clj]

   :run-fn            [:AT-RUNTIME #(deref (resolve 'minitest.runner/run))]
   :execute-fn        ::not-set! ;; Set in function of the :lang context.
   :report-fn         [:AT-RUNTIME #(deref (resolve 'minitest.reporter/report))]
   :orchestrate-fn    [:AT-RUNTIME #(deref (resolve 'minitest.orchestrator/orchestrate))]


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

   :CTX  {:exec-mode      :on-eval
          :env            :dev
          :js-env         :node
          :last-in-level  ::not-set
          :first-in-level ::not-set!
          :test-level     ::not-set!
          :status         ::not-set!
          :dots           false}
   :WHEN (let [silent-success
               {:WHEN {:status    {:success {:silent    true}}}}
               run-on-load
               {:WHEN {:exec-mode {:on-load {:run-tests true}}}}]
           ;; reads as:
           ;; when       is    then         is
           {:lang       {:clj  {:execute-fn [:AT-RUNTIME #(deref (resolve 'minitest.executor/execute-clj))]}
                         :cljs {:execute-fn [:AT-RUNTIME #(deref (resolve 'minitest.executor/execute-cljs))]}}
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
            :status     {:success     {:logo           "✅"}
                         :failure     {:logo           "❌"}
                         :error       {:logo           "🔥"}}
            :test-level {:ns          {:post-separator "\n"}
                         :block       {:post-separator "\n"}
                         :stats       {:separator      "  "
                                       :WHEN {:dots {true {:separator ""}}}
                                       :post-separator "\n"}}})})
