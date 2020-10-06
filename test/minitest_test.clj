(ns minitest-test
  (:require [clojure.test   :refer [deftest testing is]]
            [clojure.string :as str]
            [minitest       :refer [tests test! *tests* *currently-loading*
                                    *config*]]))

; (defmacro with-out-str [& args]
;   `(let [out# (clojure.core/with-out-str ~@args)]
;      (println "*** Printed:")
;      (print out#)
;      out#))

(deftest test-on-load
  (reset! *tests* {})
  (let [printed (with-out-str (require 'minitest-test-namespace :reload))]
    (testing "tests are registered"
      (is (> (count (get @*tests* 'minitest-test-namespace)) 0)))
    (testing "tests are not run"
      (is (= "" printed)))))

(defn- second-form-in-file [f]
  (-> (str \[ (slurp f) \])
      read-string
      second))

(deftest test-on-eval
  (reset! *tests* {})
  (let [printed (with-out-str
                  (binding [*currently-loading* false]
                    (-> (second-form-in-file "test/minitest_test_namespace.clj")
                        eval)))]
    (testing "tests are not registered"
      (is (= 0 (count (get @*tests* (ns-name *ns*))))))
    (testing "tests are run once"
      (is (= 1 (count (re-seq #"\(inc 1\) => 2" printed)))))))

(deftest test-load-tests
  (testing "when clojure.test/*load-tests* is false"
    (binding [clojure.test/*load-tests* false]
      (reset! *tests* {})
      (let [printed (with-out-str (require 'minitest-test-namespace :reload))]
        (testing "tests are not registered"
          (is (= 0 (count (get @*tests* 'minitest-test-namespace)))))
        (testing "tests are not run"
          (is (= "" printed)))))))


;; - [√] A "bug". We don't want to have to order the tests any differently
;;       than the rest of the code
(declare inc-it*)

(defn inc-it [x]
  (inc-it* x))

(tests (inc-it 1) => 2)
(tests (inc-it 2) => 3)

(defn inc-it* [x]
  (inc x))
;; Raises: Attempting to call unbound fn: #'minitest/inc-it*.
;; Solution: run the tests after clojure.core/load is done defining the vars.


;; - [√] Tests can refer to the lexical environment
(let [a 1]
  (tests a => 1))


(clojure.test/run-tests)
(test!)
