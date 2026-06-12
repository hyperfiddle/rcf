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
  ;; inc-report-counter bridges RCF's custom report types into clojure.test's pass/fail
  ;; tally, so runner-based execution (bb/runner, cognitect test-runner) fails on a broken
  ;; RCF assertion instead of silently passing. No-op when no run is active (REPL — the
  ;; counters ref is unbound). Mirrors the cljs reporter and dopamine-reporter below.
  {:pass (fn [_m]
           (t/inc-report-counter :pass)
           (when (thread-bound? #'*block-state*)
             (swap! *block-state* update :pass inc)))
   :fail (fn [m]
           (t/inc-report-counter :fail)
           (when (thread-bound? #'*block-state*)
             (swap! *block-state*
               (fn [s] (-> s
                         (update :fail inc)
                         (update :fails conj
                           ;; snapshot the report-time dynamics (doc string = testing
                           ;; context, enclosing deftest var) — both are popped by the
                           ;; time :summary prints, so :summary cannot read them itself
                           (-> (select-keys m [:file :line :column :expected :actual])
                               (assoc :doc      (when (seq t/*testing-contexts*) (t/testing-contexts-str))
                                      :test-var (some-> (first t/*testing-vars*) meta :name)))))))))
   :summary (fn [m]
              (let [{:keys [pass fail fails]} m]
                (when (or (pos? pass) (pos? fail))
                  (t/with-test-out
                    (when (pos? pass) (println (str "Pass: " pass)))
                    (when (pos? fail)
                      (println (str "Failed: " fail))
                      ;; location format mirrors cljs.test's testing-vars-str:
                      ;; (var) (file:line[:column])
                      (doseq [{:keys [file line column expected actual doc test-var]} fails]
                        (println (str "- " (when test-var (str "(" test-var ") "))
                                      "(" file ":" line (when column (str ":" column)) ")"))
                        (when doc (println (str "   - doc: " doc)))
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
