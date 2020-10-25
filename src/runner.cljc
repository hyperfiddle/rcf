
(declare run-execute-report!)

(defprotocol RunnerP
  (run-suite     [this ns->tests])
  (run-namespace [this ns tests])
  (run-block     [this ns cases])
  (run-case      [this exe ns case]))

(defn-    managed-ex?  [x]      (and (vector? x) (-> x first (= ::caught))))
(defn-    ex           [x]      (second x))
(defmacro managing-exs [& body] `(try ~@body
                                   (catch Throwable t#
                                     (set! *e t#)
                                     [::caught t#])))


(defn- ^:no-doc run-test-and-yield-report! [ns {:keys [test expectation] :as m}]
  (let [result   (managing-exs
                   (doto (call (:thunk test))
                         (as-> res (set! *3 *2) (set! *2 *1) (set! *1 res))))
        expected (managing-exs (call (:thunk expectation)))]
    (merge {:ns       ns
            :result   {:form (:form test)
                       :val  (when-not (managed-ex? result)   result)}
            :expected {:form (:form expectation)
                       :val  (when-not (managed-ex? expected) expected)}}
           (cond (managed-ex? expected) {:status :error  :error (ex expected)}
                 (managed-ex? result)   {:status :error  :error (ex result)}
                 (= result expected)    {:status :success}
                 :else                  {:status :failure}))))

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
  (run-case      [this exe ns case] (.execute-case exe ns case)))
