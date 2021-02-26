(ns minitest.poney
  {:minitest/config {:dots true}}
  (:require
   #?(:clj  [clojure.pprint    :refer [pprint]]
      :cljs [cljs.pprint       :refer [pprint]])
   #?(:clj  [minitest          :refer [test! tests]]
      :cljs [minitest          :refer [test!]]))

  #?(:cljs (:require-macros [net.cgrand.macrovich :as macros]
                            [minitest :refer [tests]])))

(tests 1 := 1
       2 := 0)

(tests 3 := 3
       (tests 4 := 0))

(tests 5 := 0)
