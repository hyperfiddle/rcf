(ns minitest.unify
  (:require [net.cgrand.macrovich             :as           macros]
            [clojure.walk                     :refer        [postwalk
                                                             postwalk-replace]]
            [minitest.config       #?@(:clj  [:refer        [config
                                                             context
                                                             with-config
                                                             extending-config]]
                                       :cljs [:refer        [config
                                                             context]
                                              :refer-macros [with-config
                                                             extending-config]])]
            [minitest.custom-map   #?@(:clj  [:refer        [func-map
                                                             at-runtime]]
                                       :cljs [:refer-macros [func-map
                                                             at-runtime]])]
            [minitest.higher-order #?@(:clj  [:refer        [instead-of-level|
                                                             outside-in->>
                                                             anafn
                                                             if|
                                                             anaph|]]
                                       :cljs [:refer        [instead-of-level|]
                                              :refer-macros [outside-in->>
                                                             anafn
                                                             if|
                                                             anaph|]])]
            [minitest.reporter                :refer        [print-result]]
            [minitest.utils                   :refer        [->| not| emphasize]]
            [minitest.walk                    :refer        [copostwalk]]
            [minitest              #?@(:clj  [:refer        [tests]]
                                       :cljs [:refer-macros [tests]])]
            [lambdaisland.deep-diff2          :refer        [diff]
                                              :as           diff]
            [minitest.clojure.core.unify      :refer        [unify lvar?]]))

(defn lvars [coll]
  (->> coll
       flatten
       (filter lvar?)
       set))

(defn lvar-val     [var val] [::lvar-val var val])
(defn lvar-val?    [x]       (and (coll? x)
                                  (-> x first (= ::lvar-val))))
(defn lvar-val-var [x]       (nth x 1))
(defn lvar-val-val [x]       (nth x 2))

(defn lvar-values [tested expected]
  (let [lvar? (lvars expected)]
    (->> (copostwalk (fn [t e]
                       [(if (lvar? e)
                          (lvar-val e t)
                          t)
                        expected])
                     tested expected)
         first
         (tree-seq  (every-pred coll? (not| lvar-val?))  identity)
         (filter lvar-val?)
         (group-by lvar-val-var)
         (map (fn [[var var-vals]]
                [var (set (map lvar-val-val var-vals))]))
         (into {}))))

(defn report-? [s p l n d]
  (let [ctx (context)]
    (doto d (-> (assoc :op :≈)
                (print-result :left  (-> ctx :case-data :? :tested :form)
                              :right (-> ctx :case-data :? :expected :form))))))

(defn explain-? [s p l n d]
  (let [case-data           (-> (context) :case-data :?)
        _ (println case-data)
        tested              (-> case-data :tested   :form)
        expected            (-> case-data :expected :form)
        var-vals            (lvar-values tested expected)
        errors              (filter #(-> % second count (> 1)) var-vals)
        binds-no-op         (->> (for [v (lvars expected)]
                                   [v '_])
                                 (into {}))
        lvar?               (set (keys var-vals))
        lvars-disabled      (postwalk-replace binds-no-op expected)
        tested-with-lvars   (first (copostwalk (fn [t e]
                                                 (if (lvar? e)
                                                   [e e]
                                                   [t e]))
                                               tested expected))]
    (when (seq errors)
      (println (emphasize    "   Unification conflicts:"))
      (doseq [[var vals] errors
              :let [first-val (first vals)]]
        (doseq [val vals]
          (println (str (if (= val first-val) "     • " "       ")
                        var ": " val)))))
    (when-not (unify tested lvars-disabled)
      (println (emphasize    "   Inequalities:"))
      (print            "     • ")
      (diff/pretty-print (diff tested-with-lvars expected)
                         #?(:cljs (diff/printer {:print-color false}))))
    d))

(macros/deftime
  (defmacro ? [tested expected]
    `(extending-config
         {:CTX       {:case-data
                      {:? {:tested   {:form '~tested}
                           :expected {:form '~expected}}}}
          :report-fn (outside-in->>
                       (instead-of-level|
                         [:report  :case] report-?)
                       (instead-of-level|
                         [:explain :case] (anaph|
                                            (if| (-> ~'&data :status (= :failure))
                                              explain-?
                                              (-> ~'&config :report-fn))))
                       (-> ~'&config :report-fn))}
         (tests :? (unify ~tested ~(postwalk #(if  (lvar? %)  `(quote ~%)  %)
                                       expected))))))
