(ns minitest.reporter
  (:require [clojure.string                   :as           str]
            [clojure.pprint                   :as           pp
                                              :refer        [pprint]]
            [minitest.higher-order #?@(:clj  [:refer        [on-level|
                                                             on-config|
                                                             apply|
                                                             chain|
                                                             outside-in->>
                                                             anafn
                                                             anaph|
                                                             when|
                                                             with-context|]]
                                       :cljs [:refer        [on-level|
                                                             on-config|
                                                             apply|
                                                             chain|]
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
            [minitest.utils                   :refer        [call ->| dissoc-in]]
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

(defn- print-result [report logo left-ks right-ks]
  (let [left  (get-in report left-ks)
        right (get-in report right-ks)
        s     (case (:op report)
                := (str logo " " (pr-str left) " := " (pr-str right)  "\n")
                :? (str logo " " (pr-str left) " ?"                   "\n"))]
    ;; TODO
    (if-not (ugly? s)
      (print s)
      (do
        (printab logo (pprint-str left))
        (newline)
        (printab (str (apply  str  (repeat (+ 2 (count (str logo))) \space))
                      (case (:op report) := ":= " :? "?"))
                 (when (= := (:op report))
                   (pprint-str right)))
        (newline)))))

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

(defn separating-levels| [f]
  (fn [state position level ns data]
    (when (= position :before)
      (when (not= level :case)
        (swap! befores update level conj
               (map-data :reported level data)))
      (when-let [sep (-> (config) :separator)]
        (print sep)))

    (let [new-data (f state position level ns data)
          sep      (when (and (= position :after)
                              (or (= level :case)
                                  (not= (-> befores deref level peek)
                                        (map-data :reported level data))))
                     (-> (config) :separator))]
      (try (do (when sep  (print sep))
               new-data)
        (finally
          (when (and sep (not= level :case))
            (swap! befores update level pop)))))))

(defn announce-suite [state position level ns data]
  (let [ns->tests data
        ns-cnt    (count ns->tests)
        ts-cnt    (->> ns->tests vals (apply concat) (apply concat)
                       (filter (->| :type #{:expectation}))
                       count)]
    (newline)
    (once
      state [:announced-suite]
      (println
        "-- Running minitest on"
        ns-cnt (-> "namespace" (pluralize ns-cnt))
        (str \( ts-cnt \space (-> "test" (pluralize ts-cnt)) \)))))
  data)

(defn announce-ns [state position level ns data]
  (let [tests  data
        tst-cnt (->> tests (apply concat)
                    (filter (->| :type #{:expectation}))
                    count)]
    (println "---- Testing" ns
             (str "(" tst-cnt \space
                  ;; TODO: replace with once.
                  (once-else state [:announced-nss ns] "" "more ")
                  (-> "test" (pluralize tst-cnt))
                  ")")))
  data)

(defn report-expectation [report]
  (let [status   (:status report)
        logo     (-> (config) :logo)
        left-ks  [:tested :form]
        right-ks [:expected :val]
        left     (get-in report left-ks)]
    (print-result report logo left-ks right-ks))
  report)

(defn announce-effect [data & {:keys [force printer] :or {printer println}}]
  ;; TODO: use lay for config
  (if (or force (-> (config) :effects :show-form))
    (let [logo (-> (config) :logo)]
      (printer logo (:form data))
        (assoc data :showed-form true))
    data))

(defn report-effect [data]
  (if (and (-> (config) :effects :show-result))
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
                                        pp/code-dispatch]
                                (-> data :tested :val pprint-str))
                       left-n (count
                                (str logo " " (pprint-str left)))
                       prompt (str "Actual: ")
                       s      (str prompt v)]
                   (if-not (ugly? s)
                     (print s)
                     (do (printab prompt v)
                         (newline)))))
      (macros/case :clj (flush-outputs))
      data)))

(defn do-nothing
  ([s l n d]    d)
  ([s p l n d]  d))

(macros/deftime
  (defmacro <-| [& body] `(fn [& args#] ~@body)))

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
                                    (marking-as| actioned (report-via| action)))
                                  #(concat [&state &position &level &ns]
                                           [%])))))
        report+explain (->|         (do| :report)
                            (apply| (do| :explain))
                            last)]
    (outside-in->>
      (on-level| [:before  :suite] announce-suite)
      (on-level| [:before  :ns]    announce-ns)
      (on-level| [:after   :case]  (report-via| :do))
      (on-level| [:do      :case]  report+explain)
      (on-level| [:report  :case]  report-case)
      (on-level| [:explain :case]  explain-case))))

;; --- LEVEL CONTROL
(defn continue-when| [pred continue]
  (fn [& args]
    (if (apply pred       args)
      (apply   continue   args)
      (apply   do-nothing args))))

(defn stop-when| [pred continue]
  (continue-when| (complement pred) continue))

(def ^:dynamic *reporting-level* {})

(defn stop-when-not-at-level| [f]
  (anaph|
    (fn [& args]
      (if-let
        [disabled-actions
         (and (= [&position &level] [:do :case])
              (let [conf (config)]
                (seq (filter
                       #(or (not (-> conf % :enabled))
                            (not (contains?
                                   (set [:case (*reporting-level* %)])
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

(defn handle-at-level| [f]
  (outside-in->>
    (on-level| (anafn (and (= &position :after) (not= &level :case)))
      (anafn
        (let [levels (-> &state deref :later-at-level)]
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
                (with-config {:execute-fn do-nothing}
                  ((-> (config) :run-fn)  &state &level &ns &data))))
            &data))))
    f))

(defn reprint-report-with-explanation| [f]
  (outside-in->>
    (on-level| [:explain :case]
      (when| (and (seq *reporting-level*)
                  (not= (or (-> &data :later-at-level :report)  :case)
                        (or (-> &data :later-at-level :explain) :case)))
        (with-context| {:report {:enabled true}}
          (chain| (anafn (dissoc &data :reported))
                  (report-via| :report)))))
    f))

(defn handling-reports-at-level| [f]
  (outside-in->>
    mark-for-later|
    stop-when-not-at-level|
    handle-at-level|
    reprint-report-with-explanation|
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
      (when| (and (-> (config) :stats :enabled)
                  (not (-> &data :stats-counted)))
        (marking-as| :stats-counted
          increment-stats)))
    (chain| f
            (on-level|
              (anafn (and (-> (config) :stats :enabled)
                          (= [&position &level]
                             [:after (-> (config) :stats :level)])))
              display-stats))))

;; --- DOTS MODE
(defn report-case-as-dot [state position level ns data]
  (print (-> (config) :logo))
  data)

(defn handling-dots-mode| [continue]
  (let [handle| #(on-config| {:dots true}
                   (when| (-> &data %1 not)
                     (marking-as| %1 %2)))]
    (outside-in->>
      (on-level| [:report  :case] (handle| :reported  report-case-as-dot))
      (on-level| [:explain :case] (handle| :explained explain-case))
      continue)))

;; --- Main entry point
(def report
  (outside-in->> binding-test-output|
                 handling-stats-at-stats-level|
                 handling-silent-mode|
                 handling-dots-mode|
                 separating-levels|
                 handling-reports-at-level|
                 base-report))
