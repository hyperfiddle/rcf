(ns hyperfiddle.rcf.reporters
  (:require [cljs.test :as t]))

(defn testing-vars-str
  "Returns a string representation of the current test.  Renders names
  in *testing-vars* as a list, then the source file and line of
  current assertion."
  [m]
  (let [{:keys [file line column]} m]
    (str file ":" line (when column (str ":" column)))))

;; For js console.
(defmethod t/report [::t/default :hyperfiddle.rcf/pass] [m]
  (t/inc-report-counter! :pass)
  (js/console.log "✅"))

(defmethod t/report [::t/default :hyperfiddle.rcf/fail] [m]
  (t/report (assoc m :type :fail)))
