(ns hyperfiddle.rcf.reporters
  (:require [clojure.test :as t]
            [clojure.string :as str]))

(defmethod t/report :hyperfiddle.rcf/pass [_m]
  (t/with-test-out
    (t/inc-report-counter :pass)
    (print "✅")))

(defmethod t/report :hyperfiddle.rcf/fail [m]
  (print "❌ ")
  (print (str/triml (with-out-str
                      (binding [t/*test-out* *out*]
                        (t/report (assoc m :type :fail)))))))
