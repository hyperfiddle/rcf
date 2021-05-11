(ns minitest.clojure.test
  (:require [clojure.test                      :as           test]
            [clojure.string                    :as           str]
            [clojure.template                  :as           temp]
            [minitest               #?@(:clj  [:refer        [tests]]
                                        :cljs [:refer-macros [tests]])]
            [minitest.configuration #?@(:clj  [:refer        [config
                                                              context
                                                              ns-config!
                                                              with-config
                                                              with-context]]
                                        :cljs [:refer        [config
                                                              context
                                                              ns-config!]
                                               :refer-macros [with-config
                                                              with-context]])]
            [minitest.higher-order  #?@(:clj  [:refer        [instead-of-level|
                                                              outside-in->>
                                                              minifn]]
                                        :cljs [:refer        [instead-of-level|]
                                               :refer-macros [outside-in->>
                                                              minifn]])]))

(defn handling-test-hook| [f]
  (outside-in->>
    (instead-of-level| :ns (minifn (if-let [v (ns-resolve &ns 'test-ns-hook)]
                                     (@v)
                                     (apply f &args))))
    f))

(defmacro deftest [name & body]
  `(do (defn ~name []
         (with-context {::deftest-names (concat (-> (context) ::deftest-names)
                                                ['~name])}
           (tests (do ~@body))))
       (tests (~name))))

(defmacro testing [string & body]
  `(with-context {::testing-contexts (concat (-> (context) ::testing-contexts)
                                             [~string])}
     ~@body))

(defn locate-is [state position level ns data]
  (when-let [ctxs (and (-> data :status (not= :success))
                       (seq (concat [\[] (-> (context) ::deftest-names) [\]]
                                    (-> (context) ::testing-contexts)
                                    [(-> (context) ::is-msg)])))]
    (println (str/join \space ctxs)))
  data)

(defmacro is
  ([form]     `(is ~form nil))
  ([form msg] `(with-config
                 {:CTX {::is-msg ~msg}
                  :actions {:order  [:locate :output :report :explain]
                            :locate {:enabled true
                                     :level   :case
                                     :fn      locate-is}}}
                 ~(if (and (-> form seq?)
                           (-> form first (= '=)))
                    `(tests ~(nth form 1) := ~(nth form 2))
                    `(tests :? ~form)))))

;; This is one is a literal copy of clojure.test/are. It expands to
;; minitest.clojure.test/is though.
(defmacro are [argv expr & args]
  (if (or
        ;; (are [] true) is meaningless but ok
        (and (empty? argv) (empty? args))
        ;; Catch wrong number of args
        (and (pos? (count argv))
             (pos? (count args))
             (zero? (mod (count args) (count argv)))))
    `(temp/do-template ~argv (is ~expr) ~@args)
    (throw (IllegalArgumentException. "The number of args doesn't match are's argv."))))

(defmacro with-test [definition & body]
  (when-not (-> (config) :elide-tests)
    `(let [var# ~definition]
       (defn test# []
         (with-context {::deftest-names [(str var#)]}
           ;; TODO: something's off with the context
           ;; It should print the deftest-names for var-linked tests.
           (tests #_(println "ctx" (context))
                  ~@body)))
       (alter-meta! var# assoc :test test#)
       (ns-config! (or (-> (context) :ns) (ns-name *ns*))
                   {:run-fn (handling-test-hook| (-> (config) :run-fn))})
       (tests (test#)))))

(deftest test-something
  (testing "Checking whether something"
    (testing "is correct"
      (is (= (inc 1)  1))))
  (is (< (inc 1) 2) "oops")
  (are [x y] (> x y)
       0 1
       1 0
       1 1))

(with-test (def func +)
  (func 1 2) := 3
  (func 1 1) := 1)


(def run-tests minitest/test!)

(comment
  (defn test-ns-hook []
    (println "Running tests via tests hook")
    (test-something)))

;; TODO: fixtures

(ns-config! {:WHEN {:exec-mode {:on-load {:run-tests false}}}})
