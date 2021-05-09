(ns minitest-test
  (:require [net.cgrand.macrovich             :as           macros]
            [clojure.test                     :refer        [deftest testing is are
                                                             run-tests]
             :as           test]
            [clojure.pprint                   :refer        [pprint]]
            [clojure.string                   :as           str]
            #?(:cljs [cljs.reader                      :refer        [read-string]])
            [minitest           #?@(:clj     [:refer        [test!
                                                             tests]]
                                         :cljs    [:refer        [test!]
                                                   :refer-macros [tests]])]
            [minitest.config
             #?@(:clj     [:refer        [with-config
                                          with-context]]
                      :cljs    [:refer-macros [with-config
                                               with-context]])]
            [lambdaisland.deep-diff2          :refer        [diff]
             :as           diff]
            ; [minitest-test-namespace]
            [minitest-features-test]))


(defmethod test/assert-expr 'outputting? [msg form]
  (let [[_ ns file] form]
    `(let [s#        (java.io.StringWriter.)
           ns#       ~ns
           _#        (println "Examining tests for ns" ns#)
           tested#   (do (time (with-config {:print-to s#} (test! ns#)))
                         (str s#))
           expected# (slurp ~file)]
       (if (= tested# expected#)
         (test/do-report
           {:type :pass :message ~msg :expected '~form
            :actual (list '~'= tested# expected#)})
         (test/do-report
           {:type :fail-diff :message ~msg :expected '~form
            :actual (diff (str/split-lines tested#)
                              (str/split-lines expected#))})))))

(defmethod test/report :fail-diff [m]
  (test/with-test-out
    (test/inc-report-counter :fail)
    (println "\nFAIL in" (test/testing-vars-str m))
    (when (seq test/*testing-contexts*) (println (test/testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (println "expected:" (pr-str (:expected m)))
    (println "  actual:")
    (diff/pretty-print
      (:actual m)
      #?(:cljs (diff/printer {:print-color false})))))

(defn record-test-output [ns out-file]
  (let [s      (java.io.StringWriter.)
        output (do (with-config {:print-to s} (test! ns))
                   (str s))]
    (spit out-file output)
    (println "Recorded" ns "test output to" out-file)))

; (record-test-output 'minitest-features-test "test_resources/features.normal.txt")
(deftest features-test
  (is (outputting? 'minitest-features-test "test_resources/features.normal.txt")))

(run-tests)

(macros/deftime
  (defmacro our-out-str [& args]
    `(with-out-str ~@args))

  ; Comment above and uncomment below to debug the captured output
  ; (defmacro our-out-str [& args]
  ;   `(let [out# (clojure.core/our-out-str ~@args)]
  ;      (println "*** with-out-str:")
  ;      (print out#)
  ;      out#))
  )

; #?(:clj (deftest test-on-load
;           (reset! *tests* {})
;           (let [printed (with-context {:exec-mode :on-load}
;                           (our-out-str
;                             (require 'minitest-test-namespace :reload)))]
;             (testing "tests are stored"
;               (is (= 5 (->> (get @*tests* 'minitest-test-namespace)
;                             (apply concat)
;                             count))))
;             (testing "tests are not run"
;               (is (= "" printed))))))

; #?(:clj (deftest test-elide-tests
;           #_(with-context {:elide-tests true}
;             (reset! *tests* {})
;             (let [printed (our-out-str
;                             (require 'minitest-test-namespace :reload))]
;               (testing "tests are not stored"
;                 (is (= 0 (count (get @*tests* 'minitest-test-namespace)))))
;               (testing "tests are not run"
;                 (is (= "" printed)))))))

; #?(:clj (defn- nth-form-in-file [f n]
;           (-> (str \[ (slurp f) \])
;               read-string
;               (nth n)
;               (doto pprint))))

; #?(:clj (deftest test-on-eval
;           (reset! *tests* {})
;           (let [printed (our-out-str
;                           (with-context {:exec-mod :on-eval}
;                             (-> (nth-form-in-file
;                                   "test/minitest_test_namespace.cljc" 1)
;                                 eval)))]
;             (testing "tests are not stored"
;               (is (= 0 (count (get @*tests* (ns-name *ns*))))))
;             (testing "tests are run once"
;               (is (= 1 (count (re-seq #"\(inc 1\) := 2" printed))))))))

; ; - [√] A "bug". We don't want to have to order the tests any differently
; ;       than the rest of the code; i.e. tests are run after the code has
; ;       loaded.
; (declare inc-it*)

; (defn inc-it [x]
;   (inc-it* x))

; (tests (inc-it 1) := 2)
; (tests (inc-it 2) := 3
;        (println "An effect was run !")
;        (/ 1 0)    := :oops
;        (println "Another effect was run !"))

; (defn inc-it* [x]
;   (inc x))
; ; Raises: Attempting to call unbound fn: #'minitest/inc-it*.
; ; Solution: run the tests after clojure.core/load is done defining the vars.


; ; - [√] Tests can refer to the lexical environment
; (let [a 1]
;   (tests a := 1))


; ; (test!)

; (tests (inc 0) := 1
;        (inc 1) := 2)
; (tests :ok := :ok
;        :x (= 1 1))

; ; (test!)

; (run-tests)
