(ns minitest.clojure.test
  {:minitest/config {}}
  (:require   [clojure.test     #?@(:clj  [:as           test
                                           :refer        [is]]
                                    :cljs [:as           test
                                           :refer-macros [is]])]
              [clojure.pprint              :refer        [pprint]] ;; TODO: remove
    #?(:clj   [minitest                    :refer        [ns-config! config
                                                         tests with-config
                                                         with-context]]
       :cljs  [minitest                    :refer        [ns-config! config]
                                           :refer-macros [tests with-config
                                                          with-context]])
              [minitest.utils              :refer        [->| call]]
              [minitest.higher-order #?@(:clj  [:refer [on-level|
                                                        continue-when|
                                                        outside-in->>]]
                                         :cljs [:refer [on-level|
                                                        continue-when|]
                                                :refer-macros [outside-in->>]])]))

; (ns-config! {:report-fn (outside-in->>
;                           (on-level| [:do :report]
;                             (fn [s p l n d]
;                               (test/do-report
;                                 {:type (case (:type d)
;                                          :success :pass
;                                          :error   :error
;                                          :failure :fail)
;                                  :expected (-> d :expected :form)
;                                  :tested   (-> d :tested :thunk call)})))
;                           (continue-when| (fn [s p l n d]
;                                             (and (not= [p l] [:do :report]))))
;                           (-> (config) :report-fn))})

(defn is->tests [[_is form [& _msg]]]
  (if (and (sequential? form) (-> form first (= '=)))
    (->> (let [cnt (count form)]
           (for [n (range 1 cnt) :let [m (inc n)]]
             (when (< m cnt)
               [(if (= n 1) (nth form 1)) := (nth form (inc n))])))
         (apply concat))
    [:? form]))

(defmacro deftest [name & body]
  `(with-context {::block-name '~name}
     (tests ~@(mapcat (fn [form]
                        (if (and (list? form) (-> form first #{'is `test/is}))
                          (if-let [msg (and (sequential? form)
                                            (nth form 2 false))]
                            (cons `(println ~msg) (is->tests form))
                            (is->tests form))
                          [form]))
                      body))))


(deftest test-something
  (is (>= (inc 1) 2)))
