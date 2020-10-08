(ns minitest-run-tests
  (:gen-class)
  (:require [clojure.test :refer [run-tests]]
            ;; Apply the monkey-patch to clojure.load
            [minitest]))





(defn -main [& args]
  (binding [minitest/*currently-loading* true]
    ;; Load the tests (in another run of load, monkey-patched)
    (load "minitest_test")
    ;; and run them

    (minitest/with-config {:run false}
      (minitest/test! "minitest*"))))
