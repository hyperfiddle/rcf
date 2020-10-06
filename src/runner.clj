
(declare run-and-report!)

(defprotocol RunnerP
  (run-nss-tests [this ns->tests])
  (run-ns-tests  [this ns tests])
  (run-one-test  [this ns test]))

(defn-    managed-ex?  [x]      (and (vector? x) (-> x first (= ::caught))))
(defn-    ex           [x]      (second x))
(defmacro managing-exs [& body] `(try ~@body
                                   (catch Throwable t# [::caught t#])))

(defn- ^:no-doc run-test-and-yield-report! [ns {:keys [test expectation]}]
  (let [result   (managing-exs
                   (doto (call (:thunk test))
                         (as-> res (set! *3 *2) (set! *2 *1) (set! *1 res))))
        expected (managing-exs (call (:thunk expectation)))]
    (merge {:ns       ns
            :result   {:form (:form test)        :val result}
            :expected {:form (:form expectation) :val expected}}
           (cond (managed-ex? expected) {:status :error  :error (ex expected)}
                 (managed-ex? result)   {:status :error  :error (ex result)}
                 (= result expected)    {:status :success}
                 :else                  {:status :failure}))))

(defrecord Runner [opts store]
  RunnerP
  (run-nss-tests [this ns->tests] (->>
                                    ns->tests
                                    (mapv #(vector (key %) (run-and-report! %)))
                                    (into {})
                                    doall))
  (run-ns-tests  [this ns tests]  (->>
                                    tests
                                    (map #(run-and-report! [ns %]))
                                    doall))
  (run-one-test  [this ns test]   (run-test-and-yield-report! ns test)))
