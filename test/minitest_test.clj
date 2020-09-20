(ns minitest-test
  (:require [clojure.test   :refer [deftest is]]
            [clojure.string :as str]
            [minitest       :refer [tests *tests* *currently-loading*]]))

(deftest test-on-load
  (reset! *tests* {})
  (let [printed (with-out-str (require 'minitest-test-namespace :reload))]
    (testing "tests are registered"
      (is (= 1 (count (get @*tests* 'minitest-test-namespace)))))
    (testing "tests are not run"
      (is (= "" printed)))))

(deftest test-on-eval
  (reset! *tests* {})
  (let [printed (with-out-str
                  (binding [*currently-loading* false]
                    (-> (str \[ (slurp "test/minitest_test_namespace.clj") \])
                        read-string
                        last
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

(clojure.test/run-tests)
