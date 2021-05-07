(ns minitest-features-test
  {:minitest/config {:error-depth 0
                     :effects     {:show-form   true
                                   :show-result true}
                     :WHEN {:test-key   {true {:logo "✌️"}}
                            :test-level {:block {:post-separator "\n\n"}}}}}

  (:refer-clojure                       :exclude      [println])
  (:require [net.cgrand.macrovich       :as           macros]
            [minitest        #?@(:clj  [:refer        [tests]]
                                 :cljs [:refer-macros [tests]])]
            [minitest.config #?@(:clj  [:refer        [config
                                                       context
                                                       with-config
                                                       with-context]]
                                 :cljs [:refer        [config
                                                       context]
                                        :refer-macros [with-config
                                                       with-context]])]
            [minitest.unify  #?@(:clj  [:refer        [?]]
                                 :cljs [:refer-macros [?]])]
            #?@(:clj [[minitest.inner-tests :reload true]
                      [minitest.orchestrator :reload true]
                      [minitest.runner :reload true]
                      [minitest.executor :reload true]
                      [minitest.reporter :reload true]
                      [minitest.around-load :reload true]])
            )
  #?(:cljs
      (:require-macros [minitest-features-test :refer [println
                                                       inner-test-macro]])))

(defmacro println [& args]
  (with-meta `(clojure.core/println ~@args)
    {:effects {:show-form   false
               :show-result false}}))

(tests
  (println "With a ':=' expectation...")
  (println "... success")
  (inc 1) := 2
  (println "... failure")
  (inc 0) := 2
  (println "... error")
  (throw (ex-info "intentionally-raised" {})) := 1)

(tests
  (println "With a ':?' expectation...")
  (println "... success")
  :? (zero? (inc -1))
  (println "... failure")
  :? (zero? (inc 0))
  (println "... error")
  :? (throw (ex-info "intentionally-raised" {})))

(tests
  ;; Effects
  (println "This is an effect"))

(tests
  (println "*1, *2, *3 should be bound")
  (inc 0)
  (inc *1) := 2
  (inc *2) := 2
  (inc *3) := 2
  (inc *1) := 3)

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
  (inner-test-fn)
  (println "... inner inner test")
  (tests (tests 4 := 4))
  (println "... in a let form")
  (let [expected 2]
    (tests (inc 1) := expected))
  (println "... are reported coherently with surrounding effect output")
  (tests (do (println "-- before")
             (tests 0 := 0)
             (with-config {:actions {:report {:level :block}
                                     :output {:level :block}}}
               (tests (println "... and plays well with report levels")
                      100 := 100))
             (println "-- after"))))

(tests
  (println "config via case meta for...")
  (println "... :? expectation")
  :? ^{:logo "👍"} (true? true)
  (println "... := expectation")
  ^{:logo "👍"} [2] := [2]
  (println "...effect")
  ^{:logo "👍"} (do :nothing))

(def ^:dynamic *dyn-var* :root-val)

(tests (println "behavior with dynamic vars..."))
(tests (println "... when not bound")
       *dyn-var* := :root-val)
(binding [*dyn-var* :bound-val]
  (tests (println "... when bound")
         (println "...... outside tests blocks")
         *dyn-var* := :bound-val))
(tests (println   "...... inside tests blocks")
       (binding [*dyn-var* :bound-val]
         (tests *dyn-var* := :bound-val)))

(tests (println "\nbehavior with config"))
(with-config {:logo "👐"}
  (tests (println "... outside tests blocks")
         1 := 1))
(tests (println   "... inside tests blocks")
       (with-config {:logo "👐"}
         (tests 2 := 2)))

(tests (println "\nbehavior with context"))
(with-context {:test-key true} ;; leads to {:logo "✌️"}
  (tests (println "... outside tests blocks")
         1 := 1))
(tests (println   "... inside tests blocks")
       (with-context {:test-key true}
         (tests 2 := 2)))

(tests
  (println "Unification with (? _ _) ...")
  (println "... success")
  (? [0 1 2 2] [0 ?a ?b ?b])
  (println "... failure")
  (? [-1 1 2 3 4] [0 _ ?b ?b])
  (println "... error")
  (tests (? [(throw (ex-info "intentionally-raised" {}))]
       [_])))
