(ns hyperfiddle.rcf.reporters
  (:require-macros [hyperfiddle.rcf.reporters :refer [with-reporter]])
  (:require [cljs.test :as t]))

(def ^:dynamic *reporter-key* :compact)

(defonce reporter-registry (atom {}))

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
  (set! *reporter-key* k)
  k)

(defn list-reporters [] (keys @reporter-registry))

(def ^:private block-state-path
  [:hyperfiddle.rcf.reporters/block-state])

(defn- block-state-atom []
  (get-in (t/get-current-env) block-state-path))

(def compact-reporter
  {:pass (fn [_m]
           (when-let [!s (block-state-atom)]
             (swap! !s update :pass inc)))
   :fail (fn [m]
           (when-let [!s (block-state-atom)]
             (swap! !s
               (fn [s] (-> s
                         (update :fail inc)
                         (update :fails conj
                           ;; snapshot the report-time env state (doc string = testing
                           ;; context, enclosing deftest var) — both are popped by the
                           ;; time :summary prints (:end-test-var), so capture here
                           (-> (select-keys m [:file :line :column :expected :actual])
                               (assoc :doc      (let [ctx (t/testing-contexts-str)] (when (seq ctx) ctx))
                                      :test-var (some-> (:testing-vars (t/get-current-env)) first meta :name)))))))))
   :summary (fn [m]
              (let [{:keys [pass fail fails]} m]
                (when (or (pos? pass) (pos? fail))
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
                      (println (str "   - actual: " (pr-str actual))))))))})

(def dopamine-reporter
  {:pass (fn [_m]
           (t/inc-report-counter! :pass)
           (js/console.log "✅"))
   :fail (fn [m]
           (t/report (assoc m :type :fail)))
   :summary (fn [_m])})

(register-reporter! :compact compact-reporter)
(register-reporter! :dopamine dopamine-reporter)

(defn testing-vars-str
  "Returns a string representation of the current test.  Renders names
  in *testing-vars* as a list, then the source file and line of
  current assertion."
  [m]
  (let [{:keys [file line column]} m]
    (str file ":" line (when column (str ":" column)))))

(defmethod t/report [::t/default :hyperfiddle.rcf/pass] [m]
  ((:pass (get @reporter-registry *reporter-key*)) m))

(defmethod t/report [::t/default :hyperfiddle.rcf/fail] [m]
  ((:fail (get @reporter-registry *reporter-key*)) m))

(defmethod t/report [::t/default :hyperfiddle.rcf/summary] [m]
  ((:summary (get @reporter-registry *reporter-key*)) m))

(defmethod t/report [::t/default :end-test-var] [_m]
  (when-let [!s (block-state-atom)]
    (t/report (assoc @!s :type :hyperfiddle.rcf/summary))
    (t/update-current-env! block-state-path (constantly nil))))

(defmethod t/report [:shadow.test.karma/karma :hyperfiddle.rcf/pass] [m]
  (t/report (assoc m :type :pass)))

(defmethod t/report [:shadow.test.karma/karma :hyperfiddle.rcf/fail] [m]
  (t/report (assoc m :type :fail)))
