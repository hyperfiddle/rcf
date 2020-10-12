(ns minitest-run-tests
  #?(:clj (:gen-class))
  (:require [clojure.test :refer [run-tests]]
            ;; Apply the monkey-patch to clojure.load
            [minitest]))

(defn -main [& args]
  ;; Load the tests (in another run of load, monkey-patched)
  (load "minitest_test")
  ;; and run them
  (minitest/test! "minitest*")
  (run-tests 'minitest-test))
