(ns minitest.report-at-level
  "This namespace allows one to delay the reporting of certain cases so that
  they appear at the end of their block, namespace or the whole suite.
  This can be used to separate tests that pass from those that fail, or to move
  long stacktraces at the end of the test suite for instance.

  The report level can be set for each report-action under the `:actions` key
  in the config and is sensitive to config and context values, in particular
  the `:status` of each individual case."
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
                                                              map-data
                                                              flatten-data
                                                              outside-in->>
                                                              when|
                                                              with-config|
                                                              minifn]]
                                        :cljs [:refer        [report-actions
                                                              on-level|
                                                              chain|
                                                              report-via|
                                                              do-nothing
                                                              filter-data
                                                              map-data
                                                              flatten-data]
                                               :refer-macros [outside-in->>
                                                              when|
                                                              with-config|
                                                              minifn]])]))

(def ^:dynamic *reporting-level* {})

(defn reporting-at-later-levels? []
  (seq *reporting-level*))

(defn mark-for-later|
  "At case level, before reporting later levels, add to each case datum
  something like:
  {:later-at-level {:report  {:level {:case true}}
                    :explain {:level {:block true}}}}"
  [f]
  (outside-in->>
    (on-level| [:do :case]
      (when| (not (reporting-at-later-levels?))
        (minifn (reduce (fn [d [action m]]
                          (if (and (not= (:level m) :case)
                                   (:enabled m))
                            (assoc-in d [:later-at-level action (:level m)]
                                      true)
                            d))
                        &data
                        (report-actions)))))
    f))

(defn do-nothing-when-not-at-report-level|
  "Disable report actions when it's not the right level for them."
  [f]
  (minifn
    (if-let
      [actions-to-disable
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
      (with-config {:actions (->> (for [a actions-to-disable]
                                           [a {:enabled false}])
                                         (into {}))}
        (apply f &args))
      (apply f &args))))

(defn run-reports-at-report-level| [f]
  (outside-in->>
    (on-level| (minifn (and (= &position :after) (not= &level :case)))
      (minifn
        (let [levels (->> &data
                          (map-data :later-at-level &level)
                          (flatten-data &level)
                          (filter map?)
                          (apply merge))]
          (if (some #(-> levels % &level) (keys (report-actions)))
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
                         (some (fn [[action action-conf]]
                                 (let [did-action
                                       (->> action name (str "did-") keyword)]
                                   (and (-> action-conf :enabled)
                                        (-> d :later-at-level action &level)
                                        (-> d did-action not))))
                               (report-actions))))
                     &level &data)))))
            &data))))
    f))

(defn reprint-reports-with-explanation| [f]
  (outside-in->>
    (on-level| [:explain :case]
      (when| (and (reporting-at-later-levels?)
                  (not= (or (-> &data :later-at-level :report)  :case)
                        (or (-> &data :later-at-level :explain) :case)))
        (with-config| {:actions {:report {:enabled true}}}
          (chain| (minifn (dissoc &data :did-report))
                  (report-via| :report)))));; TODO: report-via doesn't mark as reported
    f))

(defn handling-reports-at-report-level| [f]
  (outside-in->>
    ;; Collect cases to run at another level than :case
    mark-for-later|
    ;; Prevent them from running when not at the right level.
    do-nothing-when-not-at-report-level|
    ;; Instead run them at [:after :block] or [:after :ns] or [:after :suite]
    run-reports-at-report-level|
    ;; Reprint what's necessary to identify a report when moving its explanation
    ;; away from its report. TODO: need to ne generalized.
    reprint-reports-with-explanation|
    f))
