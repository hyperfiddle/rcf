(ns minitest-test-namespace
  (:require [minitest :refer [tests]]))

(tests (inc 1) => 2)
