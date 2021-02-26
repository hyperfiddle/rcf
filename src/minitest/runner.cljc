(ns minitest.runner
  (:require [clojure.pprint                   :refer        [pprint]]
            [net.cgrand.macrovich             :as           macros]
            [minitest.around-load             :refer        [*tests-to-process*
                                                             *tests*]]
            [minitest.config       #?@(:clj  [:refer        [config
                                                             with-context
                                                             with-config]]
                                       :cljs [:refer        [config]
                                              :refer-macros [with-context
                                                             with-config]])]
            [minitest.higher-order #?@(:clj  [:refer        [instead-of-level|
                                                             outside-in->>
                                                             anafn]]
                                       :cljs [:refer        [instead-of-level|]
                                              :refer-macros [outside-in->>
                                                             anafn]])]
            [minitest.utils                   :refer        [call]]
            [minitest.config                  :refer        [config]]
            [minitest.walk                    :refer        [coprewalk]])
  #?(:cljs
      (:require-macros [minitest.runner :refer        [managing-exs]])))

; TODO?
; [2021-01-07 20:54:54.775 - WARNING] :shadow.cljs.devtools.server.reload-classpath/macro-reload-ex - {:ns-sym hyperfiddle.hfql20}
; IllegalStateException Can't set!: *e from non-binding thread
; (defmacro maybe-set-*e [exception]
;   (macros/case
;     :clj  `(set! *e ~exception)
;     :cljs nil))

(defn-      managed-ex?  [x]       (and (vector? x) (-> x first (= ::caught))))
(defn-      ex           [x]       (second x))
(macros/deftime
  (defmacro managing-exs [& body] `(try ~@body
                                     (catch ~(macros/case :clj  'Throwable
                                                          :cljs 'js/Error) t#
                                       (set! *e t#)
                                       [::caught t#]))))
(defn- !1-2-3 [x]
  (set! *3 *2) (set! *2 *1) (set! *1 x)
  x)

(defn- run-effect! [ns {:keys [thunk form] :as test}]
  (assert (-> test :run not))
  (let [result (managing-exs (!1-2-3 (call thunk)))]
    (merge
      test
      {:type     :effect
       :ns       ns
       :form     form
       :location :effect
       :run      true}
      (if (managed-ex? result)
        {:status :error
         :error  (ex result)}
        {:status :success
         :result result}))))

;; TODO
; (defmacro lay [[sym expr & more-bindings] & body]
;   (let [delay-sym (gensym (str "laid-" sym "-"))]
;     `(let [~delay-sym (delay ~expr)]
;        (symbol-macrolet [~sym (deref ~delay-sym)]
;          ~@(if (empty? more-bindings)
;              body
;              `[(lay ~more-bindings ~@body)])))))


(defn- run-simple-expectation! [ns test]
  (assert (-> test :run not))
  (let
    [testedv   (delay (managing-exs (!1-2-3 (-> test :tested   :thunk call))))
     expectedv (delay (managing-exs         (-> test :expected :thunk call)))]
    (merge
      test
      {:type  :expectation
       :ns     ns
       :op     (:op test)
       :tested (merge {:form (-> test :tested :form)}
                      (when-not (managed-ex? @testedv)
                        {:val @testedv}))
       :location :expectation
       :run      :true}
      (when (= (:op test) :=)
        {:expected (merge {:form (-> test :expected :form)}
                          (when-not (managed-ex? @expectedv)
                            {:val @expectedv}))})
      (let [err-expected? (delay (and (= (:op test) :=)
                                      (managed-ex? @expectedv)))
            success?      (delay (case (:op test)
                                   :=  (= @testedv @expectedv)
                                   :?  @testedv))]
        (cond
          (managed-ex? @testedv)  {:status :error  :error (ex @testedv)}
          @err-expected?          {:status :error  :error (ex @expectedv)}
          @success?               {:status :success}
          :else                   {:status :failure})))))

(def wildcard-symbol '_)

(defn wildcard-expectation? [test]
  (and (= (:op test) :=)
       (let [expectedv (managing-exs (-> test :expected :thunk call))]
         (if (managed-ex? expectedv)
           [expectedv false]
           [expectedv (and (coll? expectedv)
                        (->> (tree-seq coll? identity expectedv)
                             (remove coll?)
                             (some #{wildcard-symbol})))]))))

(defn- run-wildcard-expectation! [ns test]
  (let [testedv     (managing-exs (!1-2-3 (-> test :tested :thunk call)))
        wildcardize (fn wildcardize [tested expected]
                      (when-let
                        [msg (cond
                               (and (map? expected)
                                    (contains? (set (keys expected))
                                               wildcard-symbol))
                               "[Minitest] Can't use wildcards as keys in maps"
                               (and (set? expected)
                                    (contains? expected
                                               wildcard-symbol))
                               "[Minitest] Can't use wildcards in sets")]
                        (throw (ex-info msg {:type :minitest/illegal-wildcard
                                             :tested   tested
                                             :expected expected})))
                      [(if (= wildcard-symbol expected) expected tested)
                       expected])
        [new-testedv _]
        (if (managed-ex? testedv)
          [testedv nil]
          (let [expectedv (-> test :expected :thunk call)
                result    (managing-exs
                            (coprewalk wildcardize testedv expectedv))]
            (if (managed-ex? result)
              [result nil]
              result)))]
    (-> (run-simple-expectation!
          ns (assoc-in test [:tested :thunk] (fn [] new-testedv)))
        ;; re-establish original value for reporting
        (assoc-in [:tested :val] testedv))))

(defn run-expectation! [ns test]
  (let [[expectedv wildcard?] (wildcard-expectation? test)
        run                   (if wildcard?
                                run-wildcard-expectation!
                                run-simple-expectation!)]
    (run ns (assoc-in test [:expected :thunk]
                      (fn [] expectedv)))))

(defn run-test-and-yield-report! [ns {:keys [type op] :as test}]
  (case type
    :effect      (run-effect!      ns test)
    :expectation (run-expectation! ns test)))

(defn orch [first? last? s l & [n d]]
  (with-context {:first-in-level first?
                 :last-in-level  last?}
    (when d
      [n ((-> (config) :orchestrate-fn) s l n d)])))

(defn process-all [s l n d]
  (if (= l :ns)
    (let [d (seq d)]
      (->>
        (concat
          [(apply             orch true  false s l    (-> d first))]
          (doall (map #(apply orch false false s l %) (-> d rest butlast)))
          [(apply             orch false true  s l    (-> d rest last))])
        (into {})))
    (doall
      (concat
        (                rest (orch true  false s l n      (-> d first)))
        (doall (mapcat #(rest (orch false false s l n  %)) (-> d rest butlast)))
        (                rest (orch false true  s l n      (-> d rest last)))))))

(def level-below
  {:suite :ns
   :ns    :block
   :block :case})

(defn base-run [state level ns data]
  (let [c (config)]
    (if (= level :case)
      ((-> c :execute-fn)   state :do :case  ns data)
      (process-all state (level-below level) ns data))))

(def ^:dynamic *running-inner-tests* false)

(defn orchestrate-inner-tests| [f]
  (outside-in->>
    (instead-of-level| :case
      (anafn
        (binding [*tests*            (atom {})
                  *tests-to-process* (atom {})]
          (let [result (binding [*running-inner-tests* true]
                         (with-context {:run-tests false}
                           (f &state &level &ns &data)))]
            (if-let [tests (-> *tests-to-process* deref (get &ns))]
              (with-meta
                (concat [result]
                        (process-all &state :case &ns (-> tests first)))
                {:minitest/inner-tests true})
              result)))))
    f))

(defn handling-inner-tests-blocks| [f]
  (outside-in->>
    (instead-of-level| [:block]
      (anafn
        (with-config
          {:orchestrate-fn (orchestrate-inner-tests|
                             (-> (config) :orchestrate-fn))}
          (->> (f &state &level &ns &data)
               (mapcat #(if (-> % meta :minitest/inner-tests)
                          % [%]))
               doall))))
    f))

(def run
  (outside-in->>
    handling-inner-tests-blocks|
    base-run))
