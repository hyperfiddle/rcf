(ns minitest.report-at-level
  (:require [minitest.utils                    :refer        [dissoc-in]]
            [minitest.configuration #?@(:clj  [:refer        [config
                                                              with-config
                                                              with-context]]
                                        :cljs [:refer        [config]
                                               :refer-macros [with-config
                                                              with-context]])]
            [minitest.higher-order  #?@(:clj  [:refer        [report-actions
                                                              on-level|
                                                              chain|
                                                              report-via|
                                                              do-nothing
                                                              filter-data
                                                              outside-in->>
                                                              when|
                                                              with-config|
                                                              minifn]]
                                        :cljs [:refer        [report-actions
                                                              on-level|
                                                              chain|
                                                              report-via|
                                                              do-nothing
                                                              filter-data]
                                               :refer-macros [outside-in->>
                                                              when|
                                                              with-config|
                                                              minifn]])]
            ))

(def ^:dynamic *reporting-level* {})

(defn stop-when-not-at-report-level| [f]
  (minifn
    (if-let
      [disabled-actions
       (and (= [&position &level] [:do :case])
            (let [conf (config)]
              (->> (filter (fn [[action action-conf]]
                             (or (not (:enabled action-conf))
                                 (not (contains?
                                        (set [:case (*reporting-level* action)])
                                        (-> action-conf :level)))))
                           (report-actions))
                   (map key)
                   seq)))]
      (with-config {:actions (->> (for [a disabled-actions]
                                           [a {:enabled false}])
                                         (into {}))}
        (apply f &args))
      (apply f &args))))

(defn mark-for-later| [f]
  (outside-in->>
    (on-level| [:do :case]
      (minifn
        (reduce (fn [d [action m]]
                  (if (and (not= (:level m) :case)
                           (:enabled m)
                           (empty? *reporting-level*))
                    (do (swap! &state assoc-in
                               [:later-at-level action (:level m)]
                               true)
                        (assoc-in d [:later-at-level action (:level m)]
                                  true))
                    d))
                &data
                (report-actions))))
    f))

(defn run-reports-at-report-level| [f]
  (outside-in->>
    (on-level| (minifn (and (= &position :after) (not= &level :case)))
      (minifn
        (let [levels (-> &state deref :later-at-level)]
          (if (some #(-> levels % &level) (keys (report-actions)))
            (do
              (doseq [action (keys (report-actions))]
                (when (-> levels action &level)
                  (swap! &state dissoc-in [:later-at-level action &level])))
              (binding [*reporting-level* (reduce #(if (-> levels %2 &level)
                                                     (assoc %1 %2 &level)
                                                     %1)
                                                  *reporting-level*
                                                  (keys (report-actions)))]
                (with-context {:report-level &level}
                  (with-config {:execute-fn do-nothing}
                    ((-> (config) :run-fn)  &state &level &ns
                     (filter-data
                       (fn [d]
                         (with-context {:status (:status d) ;; TODO: case config ?
                                        :type   (:type   d)} ;; TODO: useful? add test-position?
                           (some (fn [[action m]]
                                   (let [did-action
                                         (->> action name (str "did-") keyword)]
                                     (and (-> m :enabled)
                                          (-> d :later-at-level action &level)
                                          (-> d did-action not))))
                                 (report-actions))))
                       &level &data))))))
            &data))))
    f))

(defn reprint-reports-with-explanation| [f]
  (outside-in->>
    (on-level| [:explain :case]
      (when| (and (seq *reporting-level*)
                  (not= (or (-> &data :later-at-level :report)  :case)
                        (or (-> &data :later-at-level :explain) :case)))
        (with-config| {:actions {:report {:enabled true}}}
          (chain| (minifn (dissoc &data :did-report))
                  (report-via| :report)))));; TODO: report-via doesn't mark as reported
    f))

(defn handling-reports-at-report-level| [f]
  (outside-in->>
    mark-for-later|
    stop-when-not-at-report-level|
    run-reports-at-report-level|
    reprint-reports-with-explanation|
    f))
