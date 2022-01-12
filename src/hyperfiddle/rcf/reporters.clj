(ns hyperfiddle.rcf.reporters
  (:require [clojure.stacktrace :as stack]
            [clojure.test :as t]
            [hyperfiddle.rcf.utils :as utils :refer [pprint]]))

(defmethod t/report :hyperfiddle.rcf/pass [m]
  (t/with-test-out
    (t/inc-report-counter :pass)
    (print "✅")
    #_(print "✅" (pr-str (:expected m)) '=> (pr-str (:lhs-value (:actual m))))
    ))

#_(defmethod t/report :hyperfiddle.rcf/fail [m]
  (t/with-test-out
    (t/inc-report-counter :fail)
    (prn)
    (print "❌ ")
    (println (str (testing-vars-str m) " " (t/testing-contexts-str)))
    (when (seq t/*testing-contexts*) (println (t/testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (prn)
    (pprint (:actual m))
    (prn)
    (print (case (:assert-type m)
             :<> ":="
             ":<>"))
    (prn)
    (pprint (:expected m))
    (prn)
    (prn)))

(defn testing-vars-str [m]
  (let [{:keys [ns file line]} m]
    (str
     (reverse (map #(:name (meta %)) t/*testing-vars*))
     " (" (or ns file) ":" line ")")))

(defmethod t/report :hyperfiddle.rcf/fail [m]
  (t/with-test-out
    (t/inc-report-counter :fail)
    (println "\n❌ FAIL in" (testing-vars-str m))
    (when (seq t/*testing-contexts*) (println (t/testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (println "expected:" (pr-str (:rhs-value (:actual m))))
    (println "  actual:" (pr-str (:lhs-value (:actual m))))
    (println "      in:" (pr-str (:expected m)))
    (if (not= :hyperfiddle.rcf.unify/fail (:env (:actual m)))
      (println "     env:" (pr-str (:env (:actual m)))))))

(defmethod t/report :hyperfiddle.rcf/error [m]
  (t/with-test-out
    (t/inc-report-counter :error)
    (prn)
    (print "🔥 ")
    (print (str (testing-vars-str m) " "))
    (prn)
    (when (seq t/*testing-contexts*) (println (t/testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (let [actual (:actual m)]
      (if (instance? Throwable actual)
        (stack/print-cause-trace actual t/*stack-trace-depth*)
        (pprint actual)))
    (prn)
    (print ":<> ")
    (prn)
    (pprint (:expected m))
    (prn)
    ))
