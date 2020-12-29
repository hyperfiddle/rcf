(ns minitest.poney
  (:require
   #?(:clj  [clojure.pprint    :refer [pprint]]
      :cljs [cljs.pprint       :refer [pprint]])
   #?(:clj  [minitest          :refer [test! tests with-contexts]]
      :cljs [minitest          :refer [test!]]))

  #?(:cljs (:require-macros [net.cgrand.macrovich :as macros]
                            [minitest :refer [tests with-contexts]])))

(defn entry-point []
  (println "NODEJS ENTRY POINT"))

(tests (inc 1) := 2)
