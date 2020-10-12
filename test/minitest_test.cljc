(ns minitest-test
  (:require [clojure.string    :as    str]
            [clojure.test      :refer [deftest testing is]]
   #?(:clj  [clojure.java.io   :as    io])
   #?(:cljs [cljs.reader       :refer [read-string]])
   #?(:cljs [cljs-node-io.core :refer [slurp]])
            [minitest          :refer [tests test! *tests* *currently-loading*
                                       with-contexts]
                               :include-macros true]
            [minitest-test-namespace]))

;; To debug
; (defmacro with-out-str [& args]
;   `(let [out# (clojure.core/with-out-str ~@args)]
;      (println "*** with-out-str:")
;      (print out#)
;      out#))

#?(:clj (deftest test-on-load
          (reset! *tests* {})
          (let [printed (with-contexts {:exec-mode :load}
                          (with-out-str
                            (require 'minitest-test-namespace :reload)))]
            (testing "tests are stored"
              (is (= 5 (->> (get @*tests* 'minitest-test-namespace)
                            (apply concat)
                            count))))
            (testing "tests are not run"
              (is (= "" printed))))))

#?(:clj (deftest test-*load-tests*
          (testing "when clojure.test/*load-tests* is false"
            (binding [clojure.test/*load-tests* false]
              (reset! *tests* {})
              (let [printed (with-out-str
                              (require 'minitest-test-namespace :reload))]
                (testing "tests are not stored"
                  (is (= 0 (count (get @*tests* 'minitest-test-namespace)))))
                (testing "tests are not run"
                  (is (= "" printed))))))))

;; TODO: set features for reader conditionnals
(defn- forms-in-file ""
  [f]
  (let [forms (with-open [r (-> (io/reader f)
                                (clojure.lang.LineNumberingPushbackReader.))]
                (loop [r r  forms []]
                  (let [start-line (.getLineNumber   r)
                        start-col  (.getColumnNumber r)
                        form       (read {:eof ::end} r)]
                    (if (= form ::end)
                      forms
                      (recur r (conj forms (with-meta form
                                             {:line start-line
                                              :col  start-col})))))))]))


(defn- nth-file-form [n f]
  (-> (str \[ (slurp f) \])
      read-string
      (nth n)
      (doto clojure.pprint/pprint)))

(deftest test-on-eval
  (reset! *tests* {})
  (let [printed (with-out-str
                    (binding [*currently-loading* false]
                      (-> (nth-file-form 1 "test/minitest_test_namespace.cljc")
                          eval)))]
    #_(testing "tests are not stored"
      (is (= 0 (count (get @*tests* (ns-name *ns*))))))
    #_(testing "tests are run once"
      (is (= 1 (count (re-seq #"\(inc 1\) => 2" printed)))))))

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


; ;; - [√] Tests can refer to the lexical environment
; (let [a 1]
;   (tests a => 1))


; (clojure.test/run-tests)
; (test!)

; (tests (inc 0) => 1
;        (inc 1) => 2)
; (test!)
