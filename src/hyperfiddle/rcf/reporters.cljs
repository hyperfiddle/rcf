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
  (print "✅")
  (string-print "\n")) ; \n seems required to flush in cljs, turning above into println. *shrug*


(defmethod t/report [::t/default :hyperfiddle.rcf/fail] [m]
  (t/report (assoc m :type :fail)))

;; Shadow cljs + karma test runner
(defmethod t/report [:shadow.test.karma/karma :hyperfiddle.rcf/pass] [m]
  (t/report (assoc m :type :pass)))

(defmethod t/report [:shadow.test.karma/karma :hyperfiddle.rcf/fail] [m]
  (t/report (assoc m :type :fail)))
