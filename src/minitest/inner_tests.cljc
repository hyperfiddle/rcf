(ns minitest.inner-tests
  (:require [net.cgrand.macrovich             :as           macros]
            [clojure.string                   :as           str]
            [minitest.around-load             :refer        [*tests-to-process*]]
            [minitest.orchestrator            :refer        [run-execute-report!]]
            [minitest.higher-order #?@(:clj  [:refer        [report-actioneds
                                                             stop-when|
                                                             chain|
                                                             do-nothing
                                                             outside-in->>
                                                             anafn
                                                             when|
                                                             if|]]
                                       :cljs [:refer        [report-actioneds
                                                             stop-when|
                                                             chain|
                                                             do-nothing]
                                              :refer-macros [outside-in->>
                                                             anafn
                                                             when|
                                                             if|]])]
            [minitest.config       #?@(:clj  [:refer        [config
                                                             context
                                                             with-context]
                                              :as           config]
                                       :cljs [:refer        [config
                                                             context]
                                              :refer-macros [with-context]
                                              :as           config])]
            [minitest.utils        #?@(:clj  [:refer        [gen-uuid
                                                             dmerge
                                                             with-out-str+result]]
                                       :cljs [:refer        [gen-uuid
                                                             dmerge]
                                              :refer-macros [with-out-str+result]])])
  #?(:cljs
      (:require-macros [minitest.inner-tests  :refer        [capturing-inner-test-results]])))

(def ^:dynamic *inner-test-results* nil)

(macros/deftime
  (defmacro capturing-inner-test-results [& body]
    `(binding [*inner-test-results* (atom nil)
               *tests-to-process*   (atom nil)]
       ~@body)))

(defn store-inner-test-results! [results]
  (swap! *inner-test-results* concat results))

(defn inner-test-results []
  @*inner-test-results*)

(def ^:dynamic *upper-out*      nil)
(def ^:dynamic *prevent-report* false)

(defn executing-inner-tests| [f]
  (let [gen-inner-id        (anafn (let [id (str "minitest/inner-test-"
                                                 (gen-uuid))]
                                     (binding [#?(:clj  *out*
                                                  :cljs cljs.core/*print-fn*)
                                               *upper-out*]
                                       (println id))
                                     (assoc &data :inner-id id)))
        binding-upper-out| #(anafn (binding [*upper-out*
                                             #?(:clj  *out*
                                                :cljs cljs.core/*print-fn*)]
                                     (apply % &args)))
        execute-inners      (anafn
                              (let [[result inner-tests]
                                    (capturing-inner-test-results
                                      (let [result
                                            (binding [*prevent-report* true]
                                              (config/with-forced-context
                                                {:exec-mode :inner-test}
                                                (f &state &position &level &ns
                                                   &data)))]
                                        [result (inner-test-results)]))]
                                (if (seq inner-tests)
                                  (assoc result
                                    :type        :inner-tests
                                    :inner-tests inner-tests
                                    :executed    (every? :executed inner-tests))
                                  result)))]
    (if| (= [&position &level] [:do :case])
      (outside-in->> (if| (-> &data :type (= :effect))
                       (chain| (when| (-> (context) :exec-mode (= :inner-test))
                                 gen-inner-id)
                               (binding-upper-out| execute-inners)))
                     (if| (-> &data :type (= :expectation))
                       (chain| (when| (-> (context) :exec-mode (= :inner-test))
                                 gen-inner-id)
                               f))
                     f)
      f)))

(defn reporting-inner-tests| [f]
  (outside-in->>
    (stop-when| (anafn *prevent-report*))
    (if| (= [&position &level (-> &data :type)]
            [:do       :case  :inner-tests])
      (anafn
        (let [[inners printed]
              (reduce (fn [[acc-results acc-output] test]
                        (let [s       #?(:clj  (new java.io.StringWriter)
                                         :cljs (new goog.string/StringBuffer))
                              o       #?(:clj  s
                                         :cljs #(.append s %))
                              result  (binding [config/*forced-config*
                                                {:execute-fn do-nothing ;; TODO: remove ?
                                                 :print-to   o}]
                                        (run-execute-report! :case &ns test))
                              printed (str s)]
                          [(conj acc-results result)
                           (if (-> &data :inner-outputted)
                             (str acc-output printed)
                             (str/replace acc-output
                                          (str (:inner-id test) "\n")
                                          printed))]))
                      [[] (if (-> &data :inner-outputted)
                            ""
                            (-> &data :output))]
                      (-> &data :inner-tests))]
          (print printed)
          (merge &data
                 {:inner-outputted true
                  :inner-tests     inners
                  :later-at-level  (apply dmerge (map :later-at-level inners))}
                 (->> (for [actioned report-actioneds]
                        [actioned  (every? actioned inners)])
                      (into {}))))))
    f))
