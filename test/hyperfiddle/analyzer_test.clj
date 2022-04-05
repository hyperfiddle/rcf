(ns hyperfiddle.analyzer-test
  (:require [hyperfiddle.analyzer :as ana]
            [clojure.test :as t :refer [deftest are testing]]))

(defn roundtrip
  ([form] (roundtrip (ana/empty-env) form))
  ([env form] (ana/emit (ana/analyze env form))))

(deftest roundtrips
  (testing "hf.analyzer should parse and unparse clojure code transparently."
    (are [x y] (= y x)
      (roundtrip 1)   '1
      (roundtrip '1)  1
      (roundtrip ''1) ''1
      (roundtrip :ns/a) :ns/a
      (roundtrip '(inc 1)) '(inc 1)
      (roundtrip [1]) [1]
      (roundtrip #{1}) #{1}
      (roundtrip ()) ()
      (roundtrip '(1)) '(1)
      (roundtrip {:a 1}) {:a 1}
      (roundtrip '(do 1)) '(do 1)
      (roundtrip '(do (def a 1) a)) '(do (def a 1) a)
      (roundtrip '((def a identity) 1)) '((def a identity) 1)

      (-> (roundtrip '(def ^:macro a 1))
          (second) (meta))
      {:macro true}

      (roundtrip '(if true a b)) '(if true a b)
      (roundtrip '(if true a)) '(if true a nil)
      (roundtrip '(let [a 1
                        b 2] a)) '(let* [a 1, b 2] (do a))

      (roundtrip '(loop* [a 1] (recur a))) '(loop* [a 1] (do (recur a)))

      (roundtrip '(fn* [])) '(fn* ([] (do)))
      (roundtrip '(fn* f [])) '(fn* f ([] (do)))
      (roundtrip '(fn* [a] a)) '(fn* ([a] (do a)))

      (roundtrip '(fn* ([a] a) ([a b] a))) '(fn* ([a] (do a)) ([a b] (do a)))

      (roundtrip '(fn [a] a)) '(fn* ([a] (do a)))
      (roundtrip '(fn f [])) '(fn* f ([] (do)))

      (roundtrip '(letfn* [foo (fn* foo ([a] (inc a)))] 1)) '(letfn* [foo (fn* foo ([a] (do (inc a))))] (do 1))

      (roundtrip '(try 1)) '(try (do 1))
      (roundtrip '(try 1 (catch Err e# 2))) '(try (do 1) (catch Err e# (do 2)))

      (roundtrip '(try 1 (catch Err e# 2)
                       (catch Err2 e# 3)
                       (finally 4))) 
      '(try (do 1) (catch Err e# (do 2)) (catch Err2 e# (do 3)) (finally 4))
 
      )))

(comment
  (ana/analyze (ana/empty-env) '(do 1))
  )