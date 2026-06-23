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

(defn testing-vars-str
  "Returns a string representation of the current test.  Renders names
  in *testing-vars* as a list, then the source file and line of
  current assertion."
  [m]
  (let [{:keys [file line column]} m]
    (str file ":" line (when column (str ":" column)))))

(defn- print-failure-entry
  "Print a failure/error entry labelled by the enclosing test's own :doc metadata
   (set by hyperfiddle.rcf/tests), not cljs.test's global testing-contexts stack —
   which leaks doc-strings across interleaved async tests in node CI. Print only; the
   caller owns the report counter."
  [kind m]
  (let [{:keys [name line test-ns doc]} (meta (first (:testing-vars (t/get-current-env))))
        ;; prefer the test's source location (set by hyperfiddle.rcf/tests); m's :file/:line
        ;; are the compiled-JS coordinates, useless for navigating back to the .cljc.
        loc (if test-ns (str test-ns ":" line) (testing-vars-str m))]
    (println (str "\n" kind " in (" name ") (" loc ")"))
    (when doc (println (str "  " doc))))
  (when-let [message (:message m)] (println message))
  (println "expected:" (pr-str (:expected m)))
  (println "  actual:" (pr-str (:actual m))))

(def compact-reporter
  ;; inc-report-counter! bridges RCF's custom report types into cljs.test's pass/fail
  ;; tally, so programmatic runners that check `successful?` (shadow.test.node, which
  ;; exits on it) fail on a broken async `%`. Karma uses its own bridge (see below) and
  ;; never invokes this reporter. The counter call is a no-op when no env is bound (REPL).
  {:pass (fn [_m]
           (t/inc-report-counter! :pass)
           (when-let [!s (block-state-atom)]
             (swap! !s update :pass inc)))
   :fail (fn [m]
           (t/inc-report-counter! :fail)
           (if-let [!s (block-state-atom)]
             ;; REPL: accumulate into the block, printed together at :end-test-var.
             (swap! !s
               (fn [s] (-> s
                         (update :fail inc)
                         (update :fails conj
                           ;; snapshot the report-time env state (doc string = testing
                           ;; context, enclosing deftest var) — both are popped by the
                           ;; time :summary prints (:end-test-var), so capture here
                           (-> (select-keys m [:file :line :column :expected :actual])
                               (assoc :doc      (let [ctx (t/testing-contexts-str)] (when (seq ctx) ctx))
                                      :test-var (some-> (:testing-vars (t/get-current-env)) first meta :name)))))))
             ;; Generate-tests mode (node CI): no block aggregates, so print now —
             ;; otherwise the failure is counted but invisible.
             (print-failure-entry "FAIL" m)))
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

;; Override cljs.test's built-in :error/:fail (uncaught exceptions, plain `is`) so node CI
;; labels failures with the test's own doc rather than the leaked testing-contexts stack.
;; Scoped to the ::t/default reporter key; karma uses its own key and is unaffected.
(defmethod t/report [::t/default :error] [m]
  (t/inc-report-counter! :error)
  (print-failure-entry "ERROR" m))

(defmethod t/report [::t/default :fail] [m]
  (t/inc-report-counter! :fail)
  (print-failure-entry "FAIL" m))

(defn- enclosing-test-doc
  "The enclosing test's own :doc (set by hyperfiddle.rcf/tests), or nil. Same source as
   print-failure-entry uses for node CI — the test var's metadata, not the global
   testing-contexts stack."
  []
  (:doc (meta (first (:testing-vars (t/get-current-env))))))

(defmethod t/report [:shadow.test.karma/karma :hyperfiddle.rcf/pass] [m]
  (t/report (assoc m :type :pass)))

;; Pin the testing-context to this test's own :doc before re-dispatching to karma's
;; built-in :fail, which derives its label from the env's :testing-contexts. RCF's
;; async % rewrite breaks `testing`'s finally-pop, so contexts pile up across
;; interleaved tests and the label concatenates unrelated doc-strings (the node-side
;; leak fixed in commit 61e9c1e8a). We patch the env rather than override [::karma :fail]
;; itself because karma is the build entry ns — its defmethod loads last and would win.
(defmethod t/report [:shadow.test.karma/karma :hyperfiddle.rcf/fail] [m]
  (let [prev (:testing-contexts (t/get-current-env))]
    (t/update-current-env! [:testing-contexts]
      (constantly (if-let [doc (enclosing-test-doc)] (list doc) prev)))
    (try (t/report (assoc m :type :fail))
         (finally (t/update-current-env! [:testing-contexts] (constantly prev))))))
