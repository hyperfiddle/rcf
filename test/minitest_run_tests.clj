(ns minitest-run-tests
  ;; Apply the monkey-patch to clojure.load
  (:require [minitest]))

;; Then load the tests
(load "minitest_test")
