(ns minitest-test
  (:require [clojure.string    :as    str]
            [clojure.test      :refer [deftest testing is]]
   #?(:clj  [clojure.pprint    :refer [pprint]]
      :cljs [cljs.pprint       :refer [pprint]])
   #?(:cljs [cljs.reader       :refer [read-string]])
   #?(:clj  [minitest          :refer [test! *tests*
                                       tests with-context]]
      :cljs [minitest          :refer [test! *tests*]])
            ; [minitest-test-namespace]
            )
  #?(:clj  (:require [net.cgrand.macrovich :as macros])
     :cljs (:require-macros [net.cgrand.macrovich :as macros]
                            [minitest :refer [tests with-context]])))


(macros/deftime
  (defmacro our-out-str [& args]
    `(with-out-str ~@args))

  ; Comment above and uncomment below to debug the captured output
  ; (defmacro our-out-str [& args]
  ;   `(let [out# (clojure.core/our-out-str ~@args)]
  ;      (println "*** with-out-str:")
  ;      (print out#)
  ;      out#))
  )

#?(:clj (deftest test-on-load
          (reset! *tests* {})
          (let [printed (with-context {:exec-mode :on-load}
                          (our-out-str
                            (require 'minitest-test-namespace :reload)))]
            (testing "tests are stored"
              (is (= 5 (->> (get @*tests* 'minitest-test-namespace)
                            (apply concat)
                            count))))
            (testing "tests are not run"
              (is (= "" printed))))))

#?(:clj (deftest test-*load-tests*
          (testing "when clojure.test/*load-tests* is false"
            (binding [clojure.test/*load-tests* false]
              (reset! *tests* {})
              (let [printed (our-out-str
                              (require 'minitest-test-namespace :reload))]
                (testing "tests are not stored"
                  (is (= 0 (count (get @*tests* 'minitest-test-namespace)))))
                (testing "tests are not run"
                  (is (= "" printed))))))))

#?(:clj (defn- nth-file-form [n f]
          (-> (str \[ (slurp f) \])
              read-string
              (nth n)
              (doto pprint))))

#?(:clj (deftest test-on-eval
          (reset! *tests* {})
          (let [printed (our-out-str
                          (with-context {:exec-mod :on-eval}
                            (-> (nth-file-form
                                  1 "test/minitest_test_namespace.cljc")
                                eval)))]
            (testing "tests are not stored"
              (is (= 0 (count (get @*tests* (ns-name *ns*))))))
            (testing "tests are run once"
              (is (= 1 (count (re-seq #"\(inc 1\) := 2" printed))))))))

; - [√] A "bug". We don't want to have to order the tests any differently
;       than the rest of the code; i.e. tests are run after the code has
;       loaded.
(declare inc-it*)

(defn inc-it [x]
  (inc-it* x))

(tests (inc-it 1) := 2)
(tests (inc-it 2) := 3
       (println "An effect was run !")
       (/ 1 0)    := :oops
       (println "Another effect was run !"))

(defn inc-it* [x]
  (inc x))
; Raises: Attempting to call unbound fn: #'minitest/inc-it*.
; Solution: run the tests after clojure.core/load is done defining the vars.


; - [√] Tests can refer to the lexical environment
(let [a 1]
  (tests a := 1))


(test!)

(tests (inc 0) := 1
       (inc 1) := 2)
(tests :ok := :ok
       :? (= 1 1))

; (test!)
