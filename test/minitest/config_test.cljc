(ns minitest.config-test
  (:require [clojure.test         :refer        [deftest testing is are
                                                 run-tests]]
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
            [minitest.utils       :refer        [->|]]))


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
          (with-context {:level :suite :lang :clj}
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

(run-tests)
