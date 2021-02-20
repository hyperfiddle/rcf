(ns minitest-features-test
  {:minitest/config {:error-depth 0
                     :effects {:show-form   true
                               :show-result true}
                     :WHEN {:test-level {:block {:post-separator "\n\n"}}}}}
  (:require [minitest #?@(:clj  [:refer        [tests]]
                          :cljs [:refer-macros [tests]])]))

(tests
  ;; Effects
  ^{:report {:enabled false}} (println "This is an effect"))

(tests
  ;; Basic stuff
  ^{:report {:enabled false}} (println "This should be a success")
  (inc 1) := 2
  ^{:report {:enabled false}} (println "This should be a failure")
  (inc 0) := 2
  ^{:report {:enabled false}} (println "This should be an error")
  (throw (ex-info "intentionally-raised" {})) := 1)

(tests
  ^{:report {:enabled false}} (println "*1, *2, *3 should be bound")
  (inc 0)       ;; in effects
  (inc *1) := 2 ;; as well as in assertions
  (inc *2) := 2
  (inc *3) := 2)

(tests
  ^{:report {:enabled false}} (println "*e should be bound in effects")
  ;; TODO; errors are not printed in effects
  (throw (ex-info "intentionally raised in effect" {}))
  (ex-message *e) := "intentionally raised in effect")

(tests
  ^{:report {:enabled false}} (println "*e should be bound in tests")
  (throw (ex-info "intentionally raised in assertion" {}))  := 0
  (ex-message *e) := "intentionally raised in assertion")

(tests
  ^{:report {:enabled false}} (println "wildcards are supported...")
  ^{:report {:enabled false}} (println "... for successes")
  [1 2]                                           := [1 _]
  ^{:report {:enabled false}} (println "... for failures")
  [2 2]                                           := [1 _]
  ^{:report {:enabled false}} (println "... for errors")
  [1 (throw (ex-info "intentionally raised" {}))] := [1 _])

(tests
  ^{:report {:enabled false}} (println "wildcards are supported...")
  ^{:report {:enabled false}} (println "... for maps (success)")
  {:a 1 :b 2}                                           := {:a 1 :b _}
  ^{:report {:enabled false}} (println "... for maps (failure)")
  {:a 1 :b 2}                                           := {:a 2 :b _}
  ^{:report {:enabled false}} (println "... for maps (errors)")
  {:a 1 :b (throw (ex-info "intentionally-raised" {}))} := {:a 1 :b _})

(tests
  ^{:report {:enabled false}} (println "wildcards are not supported...")
  ^{:report {:enabled false}} (println "... as keys in maps")
  {:a 1} :=  {_ 1}
  ^{:report {:enabled false}} (println "... as elements of sets")
  #{1 2} := #{_ 2})
