(ns minitest.poney
  {:minitest/config {}}
  (:require
   #?(:clj  [clojure.pprint    :refer [pprint]]
      :cljs [cljs.pprint       :refer [pprint]])
   #?(:clj  [minitest          :refer [test! tests]]
      :cljs [minitest          :refer [test!]]))

  #?(:cljs (:require-macros [net.cgrand.macrovich :as macros]
                            [minitest :refer [tests]])))

(defn entry-point []
  (println "NODEJS ENTRY POINT"))

; (tests
;   (println "a side effect")
;   (inc 1) := 2
;   (inc 2) := 3
;   [*1 *2 *3] := [3 2 nil]
;   (/ 1 0) := 1
;   0       := 1
;   1       := 1)

; ; (test!)

(tests
  (inc 3) := 4
  [1 4]   := [1 _]
  {1 :a}  := {1 _}
  (print "A test effect !")
  #{1}    := #{_})

(tests
  1 := 1)

(tests
  1 := 2
  1 := 1
  3 := 2)
