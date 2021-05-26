(ns js-runtime-example
  (:require [hyperfiddle.rcf :refer-macros [tests]]))

(tests 1 := 1)

(defn ^:dev/before-load stop [] (set! hyperfiddle.rcf/*enabled* false))
(defn ^:dev/after-load start [] (set! hyperfiddle.rcf/*enabled* true))

(defn ^:export run-one-test []
  (set! hyperfiddle.rcf/*enabled* true)
  (tests 2 := 2))
