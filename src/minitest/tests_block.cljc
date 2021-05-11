(ns minitest.tests-block
  (:require [net.cgrand.macrovich           :as            macros]
            [clojure.pprint                 :refer         [pprint]]
            [clojure.spec.alpha             :as            s]
            [minitest.utils       #?@(:clj  [:refer        [meta-macroexpand
                                                            current-ns-name
                                                            as-form
                                                            as-thunk
                                                            as-wildcard-thunk]]
                                      :cljs [:refer-macros [current-ns-name]])]
            [minitest.around-load #?@(:clj  [:refer        [process-now!
                                                            process-later!
                                                            currently-loading?]]
                                      :cljs [:refer        [process-now!
                                                            process-later!]
                                             :refer-macros [currently-loading?]])]
            [minitest.inner-tests            :refer        [store-inner-test-results!]]
            [minitest.configuration          :as           cfg
             #?@(:cljs [:include-macros  true])]))


;; TODO: too much
(macros/deftime
  (defn conform! [spec value]
    (let [result (s/conform spec value)]
      (when (= result :s/invalid)
        (throw (ex-info
                 (binding [*print-level* 7
                           *print-namespace-maps* false]
                   (str \newline
                        (with-out-str (->> (s/explain-data spec value)
                                           (mapv (fn [[k v]]
                                                   [(-> k name keyword) v]))
                                           (into {})
                                           pprint))))
                 {:type ::cant-parse-test-case})))
      result))

  (defn- parse-tests [env block-body]
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
           ;; [[:effect '(do :something)]
           ;;  [:expectation [:= {:tested 1, :op :=, :expected 1}]]]
           (mapv
             (fn [[type x]]
               (case type
                 :effect
                 (let [xpd (meta-macroexpand env x)]
                   {:type            :effect
                    :form            (-> x as-form)
                    :macroexpanded   (with-meta (-> xpd as-form)
                                       (meta xpd))
                    :thunk           (with-meta (-> xpd as-thunk)
                                       (meta xpd))
                    :bindings        `(cfg/current-case-bindings)
                    :config-bindings `(cfg/current-config-bindings)})

                 :expectation
                 (let [[op m] x]
                   (-> (merge
                         {:type            :expectation
                          :op              op
                          :bindings        `(cfg/current-case-bindings)
                          :config-bindings `(cfg/current-config-bindings)
                          :tested
                          (let [xpd  (meta-macroexpand env (-> m :tested))]
                            {:form          (-> m :tested as-form)
                             :macroexpanded (-> xpd as-form)
                             :thunk         (-> xpd as-thunk)})}
                         (when (= op :=)
                           {:expected
                            (let [xpd (meta-macroexpand env (-> m :expected))]
                              {:form          (-> m :expected as-form)
                               :macroexpanded (-> xpd as-form)
                               :thunk         (-> xpd as-wildcard-thunk)})}))
                       ))))))))

  (defmacro tests [& body]
    (when-not (-> (cfg/config) :elide-tests)
      `(cfg/with-default-context
         {:exec-mode (if (currently-loading?) :on-load :on-eval)}
         (let [cfg#    (cfg/config)
               exmod#  (-> (cfg/context) :exec-mode)
               ns#     (current-ns-name)
               block#  ~(parse-tests &env body)]
           (when (or (:store-tests cfg#)
                     (:run-tests   cfg#))
             (process-later! ns# [block#])
             (case exmod#
               :on-load    nil
               :inner-test (doto (process-now! :case ns#)
                                 (store-inner-test-results!))
               :on-eval    (process-now! :block ns#)))
           nil)))))
