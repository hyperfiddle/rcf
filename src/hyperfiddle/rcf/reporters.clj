(ns hyperfiddle.rcf.reporters
  (:require [clojure.stacktrace :as stack]
            [clojure.test :as t]
            [hyperfiddle.rcf.utils :as utils :refer [pprint testing-vars-str]]))

(defmethod t/report :pass [_m]
  (t/with-test-out
    (t/inc-report-counter :pass)
    (print "✅")))

(defmethod t/report :fail [m]
  (t/with-test-out
    (t/inc-report-counter :fail)
    (prn)
    (print "❌ ")
    (println (str (testing-vars-str m) " "))
    (when (seq t/*testing-contexts*) (println (t/testing-contexts-str) (:doc m)))
    (when-let [message (:message m)] (println message))
    (prn)
    (pprint (:actual m))
    (prn)
    (print (case (:assert-type m)
             :<> ":="
             ":≠"))
    (prn)
    (pprint (:expected m))
    (prn)
    (prn)))

(defmethod t/report :error [m]
  (t/with-test-out
    (t/inc-report-counter :error)
    (prn)
    (print "🔥 ")
    (print (str (testing-vars-str m) " "))
    (prn)
    (when (seq t/*testing-contexts*) (println (t/testing-contexts-str) (:doc m)))
    (when-let [message (:message m)] (println message))
    (let [actual (:actual m)]
      (if (instance? Throwable actual)
        (stack/print-cause-trace actual t/*stack-trace-depth*)
        (pprint actual)))
    (prn)
    (print ":≠ ")
    (prn)
    (pprint (:expected m))
    (prn)
    ))

(defmethod t/report :begin-test-var [_m] (prn))

(defmethod t/report :end-test-var [_m]
  (print \.))
