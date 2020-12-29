
(declare run-execute-report!)

(defprotocol RunnerP
  (run-suite     [this ns->tests])
  (run-namespace [this ns tests])
  (run-block     [this ns cases])
  (run-case      [this exe ns case]))

(defn-      managed-ex?  [x]       (and (vector? x) (-> x first (= ::caught))))
(defn-      ex           [x]       (second x))
(macros/deftime
  (defmacro managing-exs [& body] `(try ~@body
                                     (catch ~(macros/case :clj  'Throwable
                                                          :cljs 'js/Error) t#
                                       (set! *e t#)
                                       [::caught t#]))))

  (defmacro lay [[sym expr & more-bindings] & body]
    (let [delay-sym (gensym (str "laid-" sym "-"))]
      `(let [~delay-sym (delay ~expr)]
         (symbol-macrolet [~sym (deref ~delay-sym)]
           ~@(if (empty? more-bindings)
               body
               `[(lay ~more-bindings ~@body)])))))


  ; (defn- ^:no-doc run-test-and-yield-report! [ns {:keys [type] :as test}]
  ;   (case type
  ;     :effect
  ;     (let [result (managing-exs (call (:thunk test)))]
  ;       (if (managed-ex? result)
  ;         (ex-info (str "Error in test effect\n"
  ;                       (with-out-str (pprint (:form test))))
  ;                  {:type     :minitest/effect-error
  ;                   :test     test
  ;                   :location :effect
  ;                   :error    (ex result)})
  ;         :minitest/effect-performed))

  ;     :expectation
  ;     (lay [testedv   (managing-exs (-> test :tested   :thunk call))
  ;           expectedv (managing-exs (-> test :expected :thunk call))]
  ;       (merge
  ;         {:ns     ns
  ;          :op     (:op test)
  ;          :tested (merge {:form (-> test :tested      :form)}
  ;                         (when-not (managed-ex? testedv)
  ;                           {:val testedv}))}
  ;         (when (= (:op test) :=)
  ;           {:expected (merge {:form (-> test :expectation :form)}
  ;                             (when-not (managed-ex? expectedv)
  ;                               {:val expectedv}))})
  ;         (lay [err-expected? (and (= (:op test) :=) (managed-ex? expectedv))
  ;               success?      (case (:op test)
  ;                               :=  (= testedv expectedv)
  ;                               :?  testedv)]
  ;           (cond
  ;             (managed-ex? testedv)   {:status :error  :error (ex testedv)}
  ;             err-expected?           {:status :error  :error (ex expectedv)}
  ;             success?                {:status :success}
  ;             :else                   {:status :failure}))))))

  (defn- ^:no-doc run-test-and-yield-report! [ns {:keys [type] :as test}]
      (case type
        :effect
        (let [result (managing-exs (call (:thunk test)))]
          (if (managed-ex? result)
            (ex-info (str "Error in test effect\n"
                          (with-out-str (pprint (:form test))))
                     {:type     :minitest/effect-error
                      :test     test
                      :location :effect
                      :error    (ex result)})
            :minitest/effect-performed))
        :expectation
        (let [testedv   (delay (managing-exs (-> test :tested   :thunk call)))
              expectedv (delay (managing-exs (-> test :expected :thunk call)))]
          (merge
            {:ns     ns
             :op     (:op test)
             :tested (merge {:form (-> test :tested      :form)}
                            (when-not (managed-ex? @testedv)
                              {:val @testedv}))}
            (when (= (:op test) :=)
              {:expected (merge {:form (-> test :expectation :form)}
                                (when-not (managed-ex? @expectedv)
                                  {:val @expectedv}))})
            (let [err-expected? (delay (and (= (:op test) :=) (managed-ex? @expectedv)))
                  success?      (delay (case (:op test)
                                                  :=  (= @testedv @expectedv)
                                                  :?  @testedv))]
              (cond
                (managed-ex? @testedv)  {:status :error  :error (ex @testedv)}
                @err-expected?          {:status :error  :error (ex @expectedv)}
                @success?               {:status :success}
                :else                   {:status :failure}))))))

(declare construct-record)
(defrecord Runner [opts store]
  RunnerP
  (run-suite     [this ns->tests]   (->> ns->tests
                                         (mapv (fn [[ns tsts]]
                                                 [ns (run-execute-report!
                                                       :namespace ns tsts)]))
                                         (into {})))
  (run-namespace [this ns tests]    (doall
                                      (map #(run-execute-report! :block ns %)
                                           tests)))
  (run-block     [this ns cases]    (doall
                                      (map #(run-execute-report! :case ns %)
                                           cases)))
  (run-case      [this ns case exe] (execute-case exe ns case)))
