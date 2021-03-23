(ns minitest.poney
  {:minitest/config {:dots false}}
  (:require
   #?(:clj  [clojure.pprint          :refer        [pprint]]
      :cljs [cljs.pprint             :refer        [pprint]])
   #?(:clj  [minitest                :refer        [tests]]
      :cljs [minitest                :refer-macros [tests]])
   #?(:clj  [minitest.unify          :refer        [?]]
      :cljs [minitest.unify          :refer-macros [?]])))

(tests 0 := 0
       (? [0 1 2 3] '[-1 ?a ?b ?b]))

; (tests
;   (tests 0 := 0)
;   (let [x 1]
;     (tests x := 1)))

; (tests 0 := 0
;        (tests 1 := 1))

; (tests 1 := 1
;        2 := 0)

; (tests 3 := 3
;        (tests 4 := 0))

; (tests 5 := 0)
