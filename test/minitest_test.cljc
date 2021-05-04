(ns minitest-test
  (:require [clojure.string       :as           str]
            [clojure.test         :refer        [deftest testing is are
                                                 run-tests]]
   #?(:clj  [clojure.pprint       :refer        [pprint]]
      :cljs [cljs.pprint          :refer        [pprint]])
   #?(:cljs [cljs.reader          :refer        [read-string]])
            [minitest #?@(:clj   [:refer        [test! tests]]
                          :cljs  [:refer        [test!]
                                  :refer-macros [tests]])]
            [minitest.config
                      #?@(:clj   [:refer        [deep-merge
                                                 config
                                                 forced-config
                                                 context
                                                 forced-context
                                                 with-config
                                                 with-forced-config
                                                 with-context
                                                 with-forced-context]]
                          :cljs  [:refer        [deep-merge
                                                 config
                                                 forced-config
                                                 context
                                                 forced-context]
                                  :refer-macros [with-config
                                                 with-forced-config
                                                 with-context
                                                 with-forced-context]])]
            [minitest.utils       :refer        [->|]]
            [minitest.runner :reload true]
            [minitest.executor :reload true]
            [minitest.reporter :reload true]
            [minitest.orchestrator :reload true]
            [minitest.config :reload true]
            [minitest :reload true]
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
                           (merge {:x :initial} (build-config v))]
               (= x (-> (read-config v) :x)))
       true     true
       false    false
       :unknown :initial))

(deftest test-contextual-config
  (testing "memoization"
    (testing "doesn't leak"
      (let [count-memo #(->> @minitest.config/config-memo
                             (map (fn [[k v]] [k (count v)]))
                             (into {}))
            original-cnts (do (config) (count-memo))
            new-cnts      (do (config) (count-memo))]
        (is (= original-cnts new-cnts)))))
  (testing "runtime computed values"
    (let [calls (atom 0)]
      (with-redefs [minitest.custom-map/map-func|
                    (fn [f]
                      (with-meta (->|  (fn [x] (swap! calls inc) x)  f)
                        {:minitest/map-func true}))]
        (require 'minitest.base-config :reload)
        (require 'minitest.config      :reload)
        (testing "are computed until the very last moment"
          (is (= false (-> (config) :fail-fast)))
          (is (= @calls 0))
          (is (= minitest.runner/run (-> (config) :run-fn)))
          (is (= @calls 1)))
        (testing "can be burried under nested maps"
          (with-context {:test-level :suite :lang :clj}
            (binding [*1 :abc]
              (is (= :abc (-> (config) :bindings (get #'*1))))
              (is (= @calls 2))))))))
  (testing "contextualisation"
    (let [base-cfg {:enabled {true  {:x true}
                              false {:x false}}}]
      (doseq [src      [:CFG :CTX :with-config :with-context]
              dst      [:WHEN :CTX>WHEN]]
        (testing (str (name src) " -> " (name dst) " -> CFG")
          (test-config (fn [v] (deep-merge (when (= src :CFG) {:enabled v})
                                           (when (= src :CTX) {:CTX {:enabled v}})
                                           (case dst
                                             :WHEN     {:WHEN base-cfg}
                                             :CTX>WHEN {:CTX {:WHEN base-cfg}})))
                       (fn [v] (cond (= src :with-config)
                                     (with-config  {:enabled v} (config))
                                     (= src :with-context)
                                     (with-context {:enabled v} (config))
                                     :else
                                     (config)))))))
    (testing "with-context -> WHEN>WHEN -> CFG"
      (let [f (fn [_v]
                {:WHEN {:optA {true  {:WHEN {:optB {true  {:x true}
                                                    false {:x false}}}}
                               false {:x false}}}})]
        (test-config f #(with-context {:optA true  :optB %}
                          (config)))
        (test-config f #(with-context {:optA %     :optB true}
                          (config)))))
    (testing "with-context -> CTX>WHEN -> WHEN -> CFG"
      (test-config (fn [_v]
                     {:CTX {:WHEN {:optA {true  {:optB true}
                                          false {:optB false}}}}
                      :WHEN {:optB {true  {:x true}
                                    false {:x false}}}})
                   #(with-context {:optA %}
                      (config))))))

(deftest test-forced-context
  (let [test-it   #(do (is (= 1 (-> (forced-context) :a)))
                       (is (= 1 (-> (context) :a))))
        test-many #(do (test-it)
                       (testing "overrides (with-context ...)"
                         (with-context {:a 2}
                           (test-it)))
                       (testing "overrides (with-config {:FORCED-CTX {...}} ...)"
                         (with-config {:FORCED-CTX {:a 2}}
                           (test-it)))
                       (doseq [src-ctx [:CTX :FORCED-CTX]
                               dst-ctx [:CTX :FORCED-CTX]]
                         (testing (str "overrides (with-config ["
                                       (name src-ctx) " -> " (name dst-ctx)
                                       "] ...)")
                           (with-config {src-ctx {:enabled true}
                                         :WHEN   {:enabled {true {dst-ctx {:a 2}}}}}
                             (test-it)))))
        test-all  #(do (test-many)
                       (testing "wrapping (with-config {:FORCED-CTX ...} ...)"
                         (with-config {:FORCED-CTX {:a 0}}
                           (test-many)))
                       (when %
                         (testing "wrapping (with-forced-context ...)"
                           (with-forced-context {:a 0}
                             (test-many)))))]
    (testing "(with-forced-context ...)"
      (with-forced-context {:a 1}
        (test-all true)))
    (testing "(with-config {:FORCED-CTX {...}} ...)"
      (with-config {:FORCED-CTX {:a 1}}
        (test-all false)))))

(deftest test-forced-config
  (let [test-it   #(do (is (= 1 (get (forced-config) :a)))
                       (is (= 1 (get (config)        :a))))
        test-many #(do (test-it)
                       (testing "overrides (with-config ...)"
                         (with-config {:a 2}
                           (test-it)))
                       (testing "overrides (with-config {:FORCED ...} ...)"
                         (with-config {:FORCED {:a 2}}
                           (test-it)))
                       (doseq [src-ctx [:CTX :FORCED-CTX]
                               dst-ctx [:CTX :FORCED-CTX]]
                         (testing (str "overrides (with-config ["
                                       (name src-ctx) " -> " (name dst-ctx)
                                       "] ...)")
                           (with-config {src-ctx {:enabled true}
                                         :WHEN   {:enabled {true {dst-ctx {:a 2}}}}}
                             (test-it)))))
        test-all  #(do (test-many)
                       (testing "wrapping (with-config {:FORCED ...} ...)"
                         (with-config {:FORCED {:a 0}}
                           (test-many)))
                       (when %
                         (testing "wrapping (with-forced-context ...)"
                           (with-forced-context {:a 0}
                             (test-many)))))]

    (testing "(with-forced-config ...)"
      (with-forced-config {:a 1}
        (test-all true)))
    (testing "(with-config {:FORCED ...} ...)"
      (with-config {:FORCED {:a 1}}
        (test-all true)))))

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

(run-tests)
