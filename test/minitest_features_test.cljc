(ns minitest-features-test
  {:minitest/config {:dots        false
                     :error-depth 0
                     :effects     {:show-form   true
                                   :show-result true}
                     :WHEN {:test-level {:block {:post-separator "\n\n"}}}}}

  (:refer-clojure :exclude [println])
  (:require [minitest        #?@(:clj  [:refer        [tests]]
                                 :cljs [:refer-macros [tests]])]
            [net.cgrand.macrovich       :as           macros])
  #?(:cljs
      (:require-macros [minitest-features-test :refer [println one-macro
                                                       inner-test-macro]])))

(defmacro println [& args]
  (with-meta `(clojure.core/println ~@args)
    {:report {:enabled false}}))

(tests
  ;; Effects
  (println "This is an effect"))

(tests
  ;; Basic stuff
  (println "This should be a success")
  (inc 1) := 2
  (println "This should be a failure")
  (inc 0) := 2
  (println "This should be an error")
  (throw (ex-info "intentionally-raised" {})) := 1)

(tests
  (println "*1, *2, *3 should be bound")
  (inc 0)       ;; in effects
  (inc *1) := 2 ;; as well as in assertions
  (inc *2) := 2
  (inc *3) := 2)

(tests
  (println "*e should be bound in effects")
  ;; TODO; errors are not printed in effects
  (throw (ex-info "intentionally raised in effect" {}))
  (ex-message *e) := "intentionally raised in effect")

(tests
  (println "*e should be bound in tests")
  (throw (ex-info "intentionally raised in assertion" {}))  := 0
  (ex-message *e) := "intentionally raised in assertion")

(tests
  (println "wildcards are supported...")
  (println "... for successes")
  [1 2]                                           := [1 _]
  (println "... for failures")
  [2 2]                                           := [1 _]
  (println "... for errors")
  [1 (throw (ex-info "intentionally raised" {}))] := [1 _])

(tests
  (println "wildcards are supported...")
  (println "... for maps (success)")
  {:a 1 :b 2}                                           := {:a 1 :b _}
  (println "... for maps (failure)")
  {:a 1 :b 2}                                           := {:a 2 :b _}
  (println "... for maps (errors)")
  {:a 1 :b (throw (ex-info "intentionally-raised" {}))} := {:a 1 :b _})

(tests
  (println "wildcards are not supported...")
  (println "... as keys in maps")
  {:a 1} :=  {_ 1}
  (println "... as elements of sets")
  #{1 2} := #{_ 2})

(macros/deftime
  (defmacro inner-test-macro []
    `(tests 2 := 2)))

(defn inner-test-fn []
  (tests 3 := 3))

(tests
  (println "inner tests ...")
  (println "... inline")
  (tests 1 := 1)
  (println "... macroexpansion")
  (inner-test-macro)
  (println "... function call")
  ^{:report {:enabled false}} (inner-test-fn)
  (println "... inner inner test")
  (tests (tests 4 := 4)))
