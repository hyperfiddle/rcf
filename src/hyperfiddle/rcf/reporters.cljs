(ns hyperfiddle.rcf.reporters
  (:require [cljs.test :as t]
            [hyperfiddle.rcf.utils :as utils :refer [pprint-str]]))

(defn testing-vars-str
  "Returns a string representation of the current test.  Renders names
  in *testing-vars* as a list, then the source file and line of
  current assertion."
  [m]
  (let [{:keys [file line column]} m]
    (str file ":" line (when column (str ":" column)) )))

;; For js console.
(defmethod t/report [::t/default :pass] [m]
  (t/inc-report-counter! :pass)
  (js/console.groupCollapsed "✅" (testing-vars-str m))
  (let [[a b] (utils/extract-comparison m)]
    (when (seq (:testing-contexts (t/get-current-env)))
      (js/console.log (t/testing-contexts-str) (:doc (t/get-current-env))))
    (js/console.log "  actual:" (pprint-str a))
    (js/console.log "expected:" (pprint-str b))
    (js/console.groupEnd)
    (when-let [message (:message m)]
      (println message))))

(defmethod t/report [::t/default :fail] [m]
  (t/inc-report-counter! :fail)
  (js/console.group (str "❌ " (testing-vars-str m)))
  (when (seq (:testing-contexts (t/get-current-env)))
    (println (t/testing-contexts-str) (:doc (t/get-current-env))))
  (when-let [message (:message m)] (println message))
  (t/print-comparison m)
  (js/console.groupEnd))

(defmethod t/report [::t/default :error] [m]
  (let [formatter-fn (or (:formatter (t/get-current-env)) pr-str)]
    (t/inc-report-counter! :error)
    (js/console.group (str "🔥" (testing-vars-str m)))
    (when (seq (:testing-contexts (t/get-current-env)))
      (println (t/testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (println "  actual:")
    (js/console.error (:actual m))
    (println "expected:" (formatter-fn (:expected m)))
    (js/console.groupEnd)))
