(ns minitest.reporter
  (:require [clojure.string                   :as           str]
            [clojure.pprint                   :as           pp
                                              :refer        [pprint]]
            [minitest.higher-order #?@(:clj  [:refer        [on-level|
                                                             on-config|
                                                             on-context|
                                                             apply|
                                                             chain|
                                                             stop-when|
                                                             do-nothing
                                                             outside-in->>
                                                             anafn
                                                             anaph|
                                                             when|
                                                             with-context|]]
                                       :cljs [:refer        [on-level|
                                                             on-config|
                                                             on-context|
                                                             apply|
                                                             chain|
                                                             stop-when|
                                                             do-nothing]
                                              :refer-macros [outside-in->>
                                                             anafn
                                                             anaph|
                                                             when|
                                                             with-context|]])]
            [minitest.config       #?@(:clj  [:refer        [config context
                                                             with-context
                                                             with-config]]
                                       :cljs [:refer        [config context]
                                              :refer-macros [with-context
                                                             with-config]])]
            [minitest.utils                   :refer        [call ->| dissoc-in
                                                             emphasize]]
            [clojure.repl                     :refer        [pst]]
            [net.cgrand.macrovich             :as           macros])
  #?(:cljs
      (:require-macros [minitest.reporter     :refer        [once once-else]])))

(defn- lines   [s]     (str/split s #"\r?\n"))
(defn- tabl   ([s]     (tabl "  " s))
              ([pad s] (->> (lines s)
                            (map #(str pad %))
                            (str/join "\n"))))
(defn- printab [tab s] (let [ls (lines s)]
                         (print tab (first ls))
                         (print (tabl (->> (repeat (count (str tab)) \space)
                                           str/join)
                                      (str/join \newline (rest ls))))))

(defn- ugly?
  ([s]            (ugly? s 1))
  ([s term-ratio] (> (count s)
                     (->> (config) :term-width (* term-ratio)))))

(defn pprint-str [x] (with-out-str (pprint x)))

(macros/deftime
  (defmacro ^:private once [state pth expr]
    `(let [state# ~state  pth# ~pth]
       (when-not (get-in @state# pth#)
         (let [result# ~expr]
           (swap! state# assoc-in pth# ::run-once)
           result#))))

  ;; TODO: necessary ?
  (defmacro ^:private once-else [state pth expr & [else]]
    `(if-let [cached# (once ~state ~pth ~expr)]
       cached#
       ~else)))

(defn pluralize [x n]
  (let [length (count x)]
    (str x (if (> n 1)
             (if (= "ss" (subs x (- length 2) length))
               "es"
               "s")
             ""))))

(defn binding-test-output| [f]
  (fn [state position level ns data]
    (binding [*out* (-> (config) :out)]
      (f state position level ns data))))

(defn filter-data [pred level data]
  (case level
    :suite (->> data
                (map (fn [[k v]]  [k (filter-data pred :ns v)]))
                (filter (->| second seq))
                (into {}))
    :ns    (->> data
                (filter (partial filter-data pred :block))
                (filter seq))
    :block (filter pred data)))

(defn map-data [pred level data]
  (case level
    :suite (->> data
                (map (fn [[k v]]  [k (map-data pred :ns v)]))
                (into {}))
    :ns    (->> data
                (map (partial map-data pred :block)))
    :block (map pred data)))

(def actions [:report :explain])

(def befores (atom {}))

;; TODO: only print before separators if somethings eventually gets printed.
;; Strategy: figure that out in [:before :suite] (i.e. before anything runs).
;; See mark-for-later| which stores this info in the :later-at-level
;; field in &data, but at the [:do :case] level.
(defn separating-levels| [f]
  (fn [state position level ns data]
    (when (= position :before)
      (when (not= level :case)
        (swap! befores update level conj
               (map-data (juxt :reported :explained) level data)))
      (when-let [sep (-> (config) :separator)]
        (print sep)))

    (let [new-data (f state position level ns data)
          sep      (when (and (= position :after)
                              (or (if (= level :case)
                                    true
                                    #_(some #(and (= (-> data :later-at-level %)
                                                   (*reporting-level* %))
                                                (-> (config) % :enabled))
                                          actions)
                                    (not= (-> befores deref level peek)
                                          (map-data (juxt :reported :explained)
                                                    level data)))))
                     (-> (config) :separator))]
      (try (do (when sep  (print sep))
               new-data)
        (finally
          (when (and sep (not= level :case))
            (swap! befores update level pop)))))))

(defn print-result [report & {:keys [left right]
                              ;; TODO: move to config
                              :or {left  (get-in report [:tested   :form])
                                   right (get-in report [:expected :val])}}]
  (let [logo     (-> (config) :logo)
        s     (if (-> report :op (= :?))
                (str logo " " (pr-str left) " ?\n")
                (str logo " "
                     (pr-str left) " " (-> report :op) " " (pr-str right)
                     "\n"))]
    (if-not (ugly? s)
      (print s)
      (do
        (printab logo (pprint-str left))
        (newline)
        (printab (str (apply  str  (repeat (+ 2 (count (str logo))) \space))
                      (if (-> report :op (= :?)) "?" (-> report :op)))
                 (when (= := (:op report))
                   (pprint-str right)))
        (newline)))))

(defn report-expectation [report]
  (doto report print-result))

(defn announce-effect [data & {:keys [force printer] :or {printer println}}]
  ;; TODO: use lay for config
  (if (or force (-> (config) :effects :show-form))
    (let [logo (-> (config) :logo)]
      (printer logo (:form data))
        (assoc data :showed-form true))
    data))

(defn report-effect [data]
  (if (or (-> (config) :effects :show-result)
          (-> data :status #{:error :failure}))
    (do (when-not (-> data :showed-form)
          (announce-effect data :force true :printer print))
        (println " ->" (:result data))
        (assoc data :showed-result true))
    data))

(macros/case
  :clj (defn- flush-outputs []
         (.flush *out*)
         (.flush *err*)))

(defn report-case [state position level ns data]
  (let [new-data (case (:type data)
                   :effect      (report-effect      data)
                   :expectation (report-expectation data))]
    (macros/case :clj (flush-outputs))
    new-data))

(defn explainable? [data]
  (or (->      data :type   (= :expectation))
      (and (-> data :type   (= :effect))
           (-> data :status (= :error)))))

(defn explain-case [state position level ns data]
  (if (not (explainable? data))
    data
    (let [status   (:status   data)
          location (:location data)
          conf     (config)
          logo     (-> conf :logo)
          left-ks  [:tested :form]
          left     (get-in data left-ks)]
      (case status
        :success nil
        :error   (macros/case
                   :clj  (binding [*err* *out*]
                           ;; TODO: set error depth in cljs
                           (pst (:error data)
                                (-> conf :error-depth))
                           (macros/case :clj (.flush *err*)))
                   :cljs (pst (:error data)))
        :failure (let [v      (binding [*print-level* 10000
                                        pp/*print-pprint-dispatch*
                                        pp/code-dispatch] ;; TODO: Keep ?
                                (-> data :tested :val pprint-str))
                       prompt (str (emphasize "   Actual: "))
                       s      (str prompt v)]
                   (if-not (ugly? s)
                     (print s)
                     (do (printab prompt v)
                         (newline)))))
      (macros/case :clj (flush-outputs))
      data)))

(defn marking-as| [k f]
  (fn [s p l n d]
    (-> (f s p l n d)
        (assoc k true))))

(defn report-via| [pos]
  (fn [s _ l n d]  ((:report-fn (config)) s pos l n d)))

(def base-report
  (let [do|            (fn [action]
                         (let [actioned (keyword (str (name action) "ed"))]
                           (anaph|
                             (->| (when| (and (-> (config) action :enabled)
                                              (-> &data actioned not))
                                    (with-context| {:report-action action}
                                      (marking-as| actioned
                                        (report-via| action))))
                                  #(concat [&state &position &level &ns]
                                           [%])))))
        report+explain (->|         (do| :report)
                            (apply| (do| :explain))
                            last)]
    (outside-in->>
      (on-level| [:after   :case]  (report-via| :do))
      (on-level| [:do      :case]  report+explain)
      (on-level| [:report  :case]  report-case)
      (on-level| [:explain :case]  explain-case))))

;; --- LEVEL CONTROL
(def ^:dynamic *reporting-level* {})

(defn stop-when-not-at-report-level| [f]
  (anaph|
    (fn [& args]
      (if-let
        [disabled-actions
         (and (= [&position &level] [:do :case])
              (let [conf (config)]
                (seq (filter
                       #(or (not (-> conf % :enabled))
                            (not (contains? (set [:case (*reporting-level* %)])
                                            (-> conf % :level))))
                       actions))))]
        (with-config (->> (for [a disabled-actions]
                            [a {:enabled false}])
                          (into {}))
          (apply f args))
        (apply f args)))))

(defn mark-for-later| [f]
  (outside-in->>
    (on-level| [:do :case]
      (anafn
        (reduce (fn [d action]
                  (let [conf           (config)
                        action-level   (-> conf action :level)
                        action-enabled (-> conf action :enabled)]
                    (if (and (not= action-level :case)
                             action-enabled
                             (empty? *reporting-level*))
                      (do (swap! &state assoc-in
                                 [:later-at-level action action-level]
                                 true)
                          (assoc-in d [:later-at-level action action-level]
                                    true))
                      d)))
                &data
                actions)))
    f))

(defn run-reports-at-report-level| [f]
  (outside-in->>
    (on-level| (anafn (and (= &position :after) (not= &level :case)))
      (anafn
        (let [levels      (-> &state deref :later-at-level)]
          (if (some #(-> levels % &level) actions)
            (do
              (doseq [action actions]
                (when (-> levels action &level)
                  (swap! &state dissoc-in [:later-at-level action &level])))
              (binding [*reporting-level* (reduce #(if (-> levels %2 &level)
                                                     (assoc %1 %2 &level)
                                                     %1)
                                                  *reporting-level*
                                                  actions)]
                (with-context {:report-level &level}
                  (with-config {:execute-fn do-nothing}
                    ((-> (config) :run-fn)  &state &level &ns
                     (filter-data
                       (fn [d]
                         (with-context {:status   (:status   d)
                                        :location (:location d)}
                           (some
                             (fn [action]
                               (let [actioned (keyword (str (name action) "ed"))]
                                 (and (-> d :later-at-level action &level)
                                      (-> d action :enabled)
                                      (-> d actioned not)
                                      false)))
                             actions)))
                       &level &data))))))
            &data))))
    f))

(defn reprint-reports-with-explanation| [f]
  (outside-in->>
    (on-level| [:explain :case]
      (when| (and (seq *reporting-level*)
                  (not= (or (-> &data :later-at-level :report)  :case)
                        (or (-> &data :later-at-level :explain) :case)))
        (with-context| {:report {:enabled true}}
          (chain| (anafn (dissoc &data :reported))
                  (report-via| :report)))));; TODO: report-via doesn't mark as reported
    f))

(defn handling-reports-at-report-level| [f]
  (outside-in->>
    mark-for-later|
    stop-when-not-at-report-level|
    run-reports-at-report-level|
    reprint-reports-with-explanation|
    f))

;; --- SILENT MODE
(defn handling-silent-mode| [f]
  (outside-in->>
    (on-level| [:do :case]
      (on-config| {:silent true}
        (when| (-> &data :reported not)
          (marking-as| :reported
            do-nothing))))
    f))

;; --- STATS
(defn increment-stats [state position level ns data]
  (let [report    data
        status    (:status report)
        stats-for (-> (config) :stats :for set)]
    (when (stats-for status)
      (swap! state update-in [:counts (:status report)]
             (fnil inc 0))))
  data)

(defn display-stats [state position level ns data]
  (with-context {:test-level :stats}
    (let [counts (:counts @state)
          ks     (keep (-> counts keys set)
                       (-> (config) :stats :for))] ;; preserve order
      (when-let [sep (with-context {:position :before} (-> (config) :separator))]
        (print sep))
      (doseq [k ks
              :let [cnt   (get counts k 0)
                    last? (= k (last ks))]
              :when (> cnt 0)]
        (print (str  cnt  " "  (pluralize (name k) cnt)  (if last? "." ", "))))
      (when-let [sep (with-context {:position :after} (-> (config) :separator))]
        (print sep))
      (swap! state dissoc :counts)))
  data)

(defn handling-stats-at-stats-level| [f]
  (outside-in->>
    (on-level| [:do :case]
      (when| (and (not (-> &data :stats-counted))
                  (-> &data :run)
                  (-> &data :type (= :expectation))
                  (-> (config) :stats :enabled))
        (marking-as| :stats-counted
          increment-stats)))
    (chain| f
            (on-level| (anafn (and (= [&position &level]
                                      [:after    (-> (config) :stats :level)])
                                   (-> (config) :stats :enabled)))
              display-stats))))

;; --- DOTS MODE
(defn report-case-as-dot [state position level ns data]
  (when (-> data :location (= :expectation))
    (print (-> (config) :logo)))
  data)

(defn handling-dots-mode| [f]
  (outside-in->>
    (on-level|  [:report :case] (on-config| {:dots true}
                                  (on-context| {:report-action :report}
                                    (when| (-> &data :reported not)
                                      (marking-as| :reported
                                                   report-case-as-dot)))))
    (stop-when| (anafn (and (= &position :report)
                            (-> (context) :report-action (= :report))
                            (-> (config) :dots))))
    f))

;; --- ANNOUNCE LEVEL
(defn announce-suite [state position level ns data]
  (let [ns-cnt (count data)]
    (newline)
    (when (= position :before)
      (println "-- Running minitest on"
               ns-cnt (-> "namespace" (pluralize ns-cnt))))
    (when (and (= position :after)
               (-> (config) :announce-nb-tests))
      (let [tst-cnt (->> data vals (apply concat) (apply concat)
                         (filter (->| :type #{:expectation}))
                         count)]
        (println "--" tst-cnt (-> "test" (pluralize tst-cnt)) "in total" level)))))

(defn announce-ns [state position level ns data]
  (when (= position :before)
    (println "---- Testing" ns))
  (when (and (= position :after)
             (-> (config) :announce-nb-tests))
    (let [tst-cnt (->> data (apply concat)
                       (filter (->| :type #{:expectation}))
                       count)]
      (println "----" tst-cnt (-> "test" (pluralize tst-cnt)) "in" ns))))

(defn announcing-level| [f]
  (chain| (anafn ((-> (config) :announce-fn) &state &position &level &ns &data)
                 &data)
          f))

;; --- Main entry point
(def report
  (outside-in->> binding-test-output|
                 separating-levels|
                 handling-reports-at-report-level|
                 handling-stats-at-stats-level|
                 handling-silent-mode|
                 handling-dots-mode|
                 announcing-level|
                 base-report))
