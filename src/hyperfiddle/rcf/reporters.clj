(ns hyperfiddle.rcf.reporters
  (:require [clojure.test :as t]))

(def ^:dynamic *block-state* nil)

(defonce reporter-registry (atom {}))

(def ^:dynamic *reporter-key* :compact)

(defn- valid-reporter? [r]
  (and (map? r)
       (every? #(fn? (get r %)) [:pass :fail :summary])))

(defn register-reporter! [k r]
  (when-not (valid-reporter? r)
    (throw (ex-info "Invalid reporter shape; require :pass :fail :summary as fns"
             {:key k :reporter r})))
  (swap! reporter-registry assoc k r)
  k)

(defn set-reporter! [k]
  (when-not (contains? @reporter-registry k)
    (throw (ex-info "Unknown reporter"
             {:key k :available (vec (keys @reporter-registry))})))
  (alter-var-root #'*reporter-key* (constantly k))
  k)

(defn list-reporters [] (keys @reporter-registry))

(defmacro with-reporter [k & body]
  `(let [k# ~k]
     (when-not (contains? @reporter-registry k#)
       (throw (ex-info "Unknown reporter"
                {:key k# :available (vec (keys @reporter-registry))})))
     (binding [*reporter-key* k#] ~@body)))

(def compact-reporter
  {:pass (fn [_m]
           (when (thread-bound? #'*block-state*)
             (swap! *block-state* update :pass inc)))
   :fail (fn [m]
           (when (thread-bound? #'*block-state*)
             (swap! *block-state*
               (fn [s] (-> s
                         (update :fail inc)
                         (update :fails conj (select-keys m [:file :line :expected :actual])))))))
   :summary (fn [m]
              (let [{:keys [pass fail fails]} m]
                (when (or (pos? pass) (pos? fail))
                  (t/with-test-out
                    (when (pos? pass) (println (str "Pass: " pass)))
                    (when (pos? fail)
                      (println (str "Failed: " fail))
                      (doseq [{:keys [file line expected actual]} fails]
                        (println (str "- " file ":" line))
                        (println (str "   - expected: " (pr-str expected)))
                        (println (str "   - actual: " (pr-str actual)))))))))})

(register-reporter! :compact compact-reporter)

(def dopamine-reporter
  {:pass (fn [_m]
           (t/inc-report-counter :pass)
           (t/with-test-out (println "✅")))
   :fail (fn [m]
           (t/report (assoc m :type :fail)))
   :summary (fn [_m])})

(register-reporter! :dopamine dopamine-reporter)

(defmethod t/report :hyperfiddle.rcf/pass [m]
  ((:pass (get @reporter-registry *reporter-key*)) m))

(defmethod t/report :hyperfiddle.rcf/fail [m]
  ((:fail (get @reporter-registry *reporter-key*)) m))

(defmethod t/report :hyperfiddle.rcf/summary [m]
  ((:summary (get @reporter-registry *reporter-key*)) m))
