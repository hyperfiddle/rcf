(ns minitest.runner
  (:require [clojure.pprint             :refer        [pprint]]
            [net.cgrand.macrovich       :as           macros]
            [minitest.config #?@(:clj  [:refer        [config
                                                       with-context]]
                                 :cljs [:refer        [config]
                                        :refer-macros [with-context]])]
            [minitest.utils             :refer        [call]]
            [minitest.config            :refer        [config]]
            [minitest.walk              :refer        [copostwalk]])
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

  ;; TODO
  ; (defmacro lay [[sym expr & more-bindings] & body]
  ;   (let [delay-sym (gensym (str "laid-" sym "-"))]
  ;     `(let [~delay-sym (delay ~expr)]
  ;        (symbol-macrolet [~sym (deref ~delay-sym)]
  ;          ~@(if (empty? more-bindings)
  ;              body
  ;              `[(lay ~more-bindings ~@body)])))))

(defn- !1-2-3 [x]
  (set! *3 *2) (set! *2 *1) (set! *1 x)
  x)

(defn- run-effect! [{:keys [thunk form] :as test}]
  (let [result (managing-exs (!1-2-3 (call thunk)))]
    (if (managed-ex? result)
      (ex-info (str "Error in test effect\n"
                    (with-out-str (pprint form)))
               {:type     :minitest/effect-error
                :test     test
                :location :effect
                :error    (ex result)})
      :minitest/effect-performed)))

(defn- run-simple-expectation! [ns test & [testedv expectedv]]
  (let [testedv
        (delay (or testedv
                   (managing-exs (!1-2-3 (-> test :tested   :thunk call)))))
        expectedv
        (delay (or expectedv
                   (managing-exs         (-> test :expected :thunk call))))]
    (merge
      {:ns     ns
       :op     (:op test)
       :tested (merge {:form (-> test :tested :form)}
                      (when-not (managed-ex? @testedv)
                        {:val @testedv}))}
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

(defn wildcard-symbol? [x]
  (= x '_))

(defn wildcard-expectation? [test]
  (and (= (:op test) :=)
       (let [expectedv (managing-exs (-> test :expected :thunk call))]
         (if (managed-ex? expectedv)
           [expectedv expectedv]
           [expectedv (and (coll? expectedv)
                        (->> (tree-seq coll? identity expectedv)
                             (remove coll?)
                             (some wildcard-symbol?)))]))))

(defn- run-wildcard-expectation! [ns test expectedv]
  (let [testedv          (managing-exs (!1-2-3 (-> test :tested :thunk call)))
        [new-testedv _]  (copostwalk
                           (fn wildcardize [tested expected]
                             [(if  (wildcard-symbol? expected)  '_  tested)
                              expected])
                           testedv expectedv)]
    (run-simple-expectation! ns test new-testedv expectedv)))

(defn run-expectation! [ns test]
  (let [[expectedv wildcard?] (wildcard-expectation? test)]
    (if wildcard?
      (run-wildcard-expectation! ns test expectedv)
      (run-simple-expectation!   ns test))))

(defn run-test-and-yield-report! [ns {:keys [type op] :as test}]
  (case type
    :effect      (run-effect!         test)
    :expectation (run-expectation! ns test)))

(defn run [state level ns data]
  (let [conf        (config)
        execute     (-> conf :execute-fn)
        orchestrate (-> conf :orchestrate-fn)
        orch        (fn [first? last? s l & [n d]]
                      (with-context {:first-in-level first?
                                     :last-in-level  last?}
                        (when d
                          [n (orchestrate s l n d)])))
        process-all
        (fn [s l n d]
          (doall
            (concat
              (         rest (orch true  false s l n      (-> d first)))
              (mapcat #(rest (orch false false s l n  %)) (-> d rest butlast))
              (         rest (orch false true  s l n      (-> d rest last))))))]
    (case level
      :suite
      (let [d (seq data)]
        (->>
          (concat
            [(apply      orch true  false state :ns           (-> d first))]
            (doall (map #(apply orch true  false state :ns %) (-> d rest butlast)))
            [(apply      orch true  false state :ns           (-> d rest last))])
          (into {})))
      :ns     (process-all state     :block ns data)
      :block  (process-all state     :case  ns data)
      :case   (execute     state :do :case  ns data))))
