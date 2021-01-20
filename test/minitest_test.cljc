(ns minitest-test
  (:require [clojure.string       :as    str]
            [clojure.test         :refer [deftest testing is are run-tests]]
   #?(:clj  [clojure.pprint       :refer [pprint]]
      :cljs [cljs.pprint          :refer [pprint]])
   #?(:cljs [cljs.reader          :refer [read-string]])
   #?(:clj  [minitest             :refer [tests test! *tests* config
                                          with-context with-config *debug*]]
      :cljs [minitest             :refer [      test! *tests* config]])
            ; [minitest-test-namespace]
            )
  #?(:clj  (:require [net.cgrand.macrovich :as macros])
     :cljs (:require-macros [net.cgrand.macrovich :as macros]
                            [minitest :refer [tests
                                              with-context with-config]])))


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

; #?(:clj (deftest test-on-load
;           (reset! *tests* {})
;           (let [printed (with-context {:exec-mode :on-load}
;                           (our-out-str
;                             (require 'minitest-test-namespace :reload)))]
;             (testing "tests are stored"
;               (is (= 5 (->> (get @*tests* 'minitest-test-namespace)
;                             (apply concat)
;                             count))))
;             (testing "tests are not run"
;               (is (= "" printed))))))

#?(:clj (deftest test-elide-tests
          #_(with-context {:elide-tests true}
            (reset! *tests* {})
            (let [printed (our-out-str
                            (require 'minitest-test-namespace :reload))]
              (testing "tests are not stored"
                (is (= 0 (count (get @*tests* 'minitest-test-namespace)))))
              (testing "tests are not run"
                (is (= "" printed)))))))

; #?(:clj (defn- nth-form-in-file [f n]
;           (-> (str \[ (slurp f) \])
;               read-string
;               (nth n)
;               (doto pprint))))

; #?(:clj (deftest test-on-eval
;           (reset! *tests* {})
;           (let [printed (our-out-str
;                           (with-context {:exec-mod :on-eval}
;                             (-> (nth-form-in-file
;                                   "test/minitest_test_namespace.cljc" 1)
;                                 eval)))]
;             (testing "tests are not stored"
;               (is (= 0 (count (get @*tests* (ns-name *ns*))))))
;             (testing "tests are run once"
;               (is (= 1 (count (re-seq #"\(inc 1\) := 2" printed))))))))

(defn test-config [build-config read-config]
  (are [v x] (with-redefs [minitest.base-config/base-config
                           #(do (merge {:x :initial}
                                       (build-config v)))]
               (= x (-> (read-config v) :x)))
       true     true
       false    false
       :unknown :initial
       ))

(deftest test-contextual-config
  (testing ":CTX activates :WHEN"
    (test-config (fn [v]
                   {:CTX {:enabled v}
                    :WHEN        {:enabled {true  {:x true}
                                            false {:x false}}}})
                 (fn [_v]
                   (config))))
  #_(testing ":CTX activates :WHEN in :CTX"
    (test-config (fn [v]
                   {:CTX {:enabled v
                          :WHEN    {:enabled {true  {:x true}
                                              false {:x false}}}}})
                 (fn [_v]
                   (config))))
  #_(testing "with-context activates :WHEN in :CTX"
    (test-config (fn [_v]
                   {:CTX {:WHEN    {:enabled {true  {:x true}
                                              false {:x false}}}}})
                 #(with-context {:enabled %}
                    (config))))
  (testing "with-context activates :WHEN in :WHEN"
    (let [f (fn [_v]
              {:WHEN {:optA {true  {:WHEN {:optB {true  {:x true}
                                                  false {:x false}}}}
                             false {:x false}}}})]
      (test-config f #(with-context {:optA true  :optB %}
                        (config)))
      (test-config f #(with-context {:optA %     :optB true}
                        (config)))))
  (testing "with-context activates :WHEN in :CTX which activates :WHEN"
    (test-config (fn [_v]
                   {:CTX {:WHEN {:optA {true  {:optB true}
                                        false {:optB false}}}}
                    :WHEN {:optB {true  {:x true}
                                  false {:x false}}}})
                 #(with-context {:optA %}
                    (config))))
  ; (testing ""
  ;   (test-config (fn [_v]
  ;                  {:CTX {:enabled true
  ;                         :WHEN {:enabled {true  {:x true}
  ;                                          false {:x false}}}}})
  ;                #(with-context {:enabled %}
  ;                   (config))))
  )

(run-tests)

; ; - [√] A "bug". We don't want to have to order the tests any differently
; ;       than the rest of the code; i.e. tests are run after the code has
; ;       loaded.
; (declare inc-it*)

; (defn inc-it [x]
;   (inc-it* x))

; (tests (inc-it 1) := 2)
; (tests (inc-it 2) := 3
;        (println "An effect was run !")
;        (/ 1 0)    := :oops
;        (println "Another effect was run !"))

; (defn inc-it* [x]
;   (inc x))
; ; Raises: Attempting to call unbound fn: #'minitest/inc-it*.
; ; Solution: run the tests after clojure.core/load is done defining the vars.


; ; - [√] Tests can refer to the lexical environment
; (let [a 1]
;   (tests a := 1))


; ; (test!)

; (tests (inc 0) := 1
;        (inc 1) := 2)
; (tests :ok := :ok
;        :x (= 1 1))

; ; (test!)
