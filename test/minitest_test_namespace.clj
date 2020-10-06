(ns minitest-test-namespace
  (:require [minitest :refer [tests]]))

(tests ;; Keep this form the second in the file
  Bla bla bla
  (inc 1) => 2
  (inc 3) => 4
  (inc 3) => 5
  nil     => nil
  nil     => true)
