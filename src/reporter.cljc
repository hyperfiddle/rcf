
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

(defn separating-levels| [f]
  (fn [state position level ns data]
    (let [ctx (context)]
      (when-let [sep   (and (= position :before) (-> (config) :separator))]
        (print sep))
      (let [new-data (f state position level ns data)]
        (when-let [sep (and (= position :after)  (-> (config) :post-separator))]
          (print sep))
        new-data))))

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

(defn- reportable? [data]
  (and (not (= data :minitest/effect-performed))
       (-> (config) :report)))

(defn report-case [state position level ns data]
  (if-not (reportable? data)
    data
    (let [report   data
          status   (:status report)
          conf     (with-context {:status status} (config))
          logo     (-> conf :logo)
          left-ks  [:tested :form]
          right-ks [:expected :val]
          left     (get-in report left-ks)]
      (print-result report logo left-ks right-ks)
      (assoc report :reported true))))

(defn explain-case [state position level ns data]
  (if-not (reportable? data)
    data
    (let [report   data
          status   (:status report)
          conf     (with-context {:status status} (config))
          logo     (-> conf :logo)
          left-ks  [:tested :form]
          left     (get-in report left-ks)]
      (case status
        :success nil
        :error   #?(:clj  (binding [*err* *out*]
                            ;; TODO: set error depth in cljs
                            (pst (:error report)
                                 (-> conf :error-depth))
                            (. *err* (flush)))
                         :cljs (pst (:error report)))
        :failure (let [v      (binding [*print-level* 10000
                                        pp/*print-pprint-dispatch*
                                        pp/code-dispatch]
                                (-> report :tested :val
                                    pprint-str))
                       left-n (count
                                (str logo " " (pprint-str left)))
                       prompt (str "Actual: ")
                       s      (str prompt v)]
                   (if-not (ugly? s)
                     (print s)
                     (do (printab prompt v)
                         (newline)))))
      (assoc report :explained true))))

(defn remove-effect-data [state position level ns data]
  (remove #{:minitest/effect-performed} data))

(defn when-not-reported| [f]
  (fn [state position level ns data]
    (if (and (map? data) (not (:reported data)))
      (f state position level ns data)
      data)))

(defn marking-as-reported| [f]
  (fn [state position level ns data]
    (-> (f state position level ns data)
        (assoc :reported true))))

(defn with-status-in-context| [f]
  (fn [state position level ns data]
    (if (= level :case)
      (with-context {:status (:status data)}
        (f state position level ns data))
      (f state position level ns data))))

(def ^:private base-report
  (let [report-via| (fn [mode]  (fn [s _p _l n d]
                                  ((:report-fn (config))  s mode :case n d)))]
    (outside-in->> (on| [:before  :suite]  announce-suite)
                   (on| [:before  :ns]     announce-ns)
                   (on| [:after   :case]   (report-via| :do))
                   (on| [:do      :case]   (when-not-reported|
                                             (fn [s p l n d]
                                               (let [rpt (report-case s p l n d)]
                                                 ((report-via| :explain)
                                                  s p l n rpt)))))
                   (on| [:explain :case]   explain-case)
                   (on| [:after   :block]  remove-effect-data))))

;; --- SILENT MODE
(defn do-nothing
  ([s l n d]    d)
  ([s p l n d]  d))

(defn handling-silent-mode| [f]
  (let [silence (on-config| {:silent true}
                  (when-not-reported|
                    (marking-as-reported|
                      do-nothing)))]
    (outside-in->> (on| [:pre  :case]  silence)
                   (on| [:do   :case]  silence)
                   (on| [:post :case]  silence)
                   f)))

;; --- LEVEL CONTROL
(defn continue-when| [pred continue]
  (fn [& args]
    (if (apply pred args)
      (apply continue args)
      (last args)))) ;; last args is data

(def ^:private ^:dynamic   *blocking-reports-below* {});true
(def ^:private ^:dynamic *unblocking-reports-below* {});TODO: remove

(defmacro upbinding [binds & body]
  `(binding ~(->> (for [[name f & args] binds]
                    [name `(~f ~name ~@args)])
                  (apply concat)
                  vec)
     ~@body))

(defn perform-at-level| [target id action & [continue]]
   (fn [s p l n d]
      (let [blocking-reports?   #(get *blocking-reports-below*   id true)
            unblocking-reports? #(get *unblocking-reports-below* id false)
            bifurcate           (fn [s p l n d]
                                  (binding [*blocking-reports-below*
                                            (assoc *blocking-reports-below* id false)
                                            *unblocking-reports-below*
                                            (assoc *unblocking-reports-below* id true)]
                                    (action s p l n d))
                                  ; (upbinding
                                  ;   [(*blocking-reports-below*   assoc id false)
                                  ;    (*unblocking-reports-below* assoc id true)]
                                  ;   (action s p l n d))
                                  )
            run-reports         (fn [s p l n d]
                                  (if (unblocking-reports?)
                                    d
                                    (bifurcate s p l n d)))
            level-below         {:suite :ns
                                 :ns    :block
                                 :block :case}
            levels-below        #(set (->> (iterate level-below %)
                                           (take-while some?)
                                           (rest)));;target not included anymore
            block-levels-below| (partial
                                  continue-when|
                                  (fn [s p l n d]
                                    (not (and (contains? (levels-below target) l)
                                              (blocking-reports?)))))
            cont                (outside-in->>
                                  block-levels-below|
                                  (on| [:after target] run-reports)
                                  (if continue continue do-nothing))]
        (cont s p l n d))))

;; --- REPORT LEVEL
(defn doing-reports-at-report-level|
  ([f] (doing-reports-at-report-level|  (-> (config) :report-at-level)  f))
  ([target-level f]
   (fn [s p l n d]
     (call (perform-at-level| target-level
                              :do-reports
                              (fn [s _p l n d]
                                (with-config {:execute-fn do-nothing
                                              :explain    false}
                                  ((-> (config) :run-fn)  s l n d)))
                              f)
           s p l n d))))

;; --- EXPLAIN LEVEL
(defn explaining-reports-at-explain-level|
  ([f] (explaining-reports-at-explain-level|  (:explain-at-level (config))  f))
  ([target-level f]
   (fn [s p l n d]
     (call (perform-at-level| target-level
                              :explain-reports
                              (fn [s _p l n d]
                                (with-config {:execute-fn do-nothing
                                              :report     false}
                                  ((-> (config) :run-fn)  s l n d)))
                              f)
           s p l n d))))

;; --- STATS
(defn increment-stats [state position level ns data]
  (let [report    data
        status    (:status report)
        stats-for (-> (config) :stats-for set)]
    (when (stats-for status)
      (swap! state update-in [:counts (:status report)]
             (fnil inc 0))))
  data)

(defn display-stats [state position level ns data]
  (with-context {:test-level :stats}
    (let [conf   (config)
          counts (:counts @state)
          ks     (keep (-> counts keys set) (-> conf :stats-for))] ;; prsv order
      (when-let [sep (-> conf :separator)]       (print sep))
      (doseq [k ks
              :let [cnt   (get counts k 0)
                    last? (= k (last ks))]
              :when (> cnt 0)]
        (print (str  cnt  " "  (pluralize (name k) cnt)  (if last? "." ", "))))
      (when-let [sep (-> conf :post-separator)]  (print sep))
      (swap! state dissoc :counts)))
  data)

(defn handling-stats-at-stats-level|
  ([f] (handling-stats-at-stats-level|  (-> (config) :stats-at-level)  f))
  ([level f]
    (if-not (-> (config) :stats)
      do-nothing
      (fn [s p l n d]
          (let [g (outside-in->> (on| [:do     :case]  increment-stats)
                                 (on| [:after  level]  display-stats)
                                 f)]
            (g s p l n d))))))

;; --- DOTS MODE
(defn report-case-as-dot [state position level ns data]
  (with-context {:status (:status data)}
    (print (-> (config) :logo)))
  data)

(defn handling-dots-mode| [f]
  (outside-in->> (on| [:do      :case]  (on-config| {:dots true}
                                          (when-not-reported|
                                            (marking-as-reported|
                                              report-case-as-dot))))
                 (on| [:explain :case]  (on-config| {:dots true}
                                          (when-not-reported|
                                            (marking-as-reported|
                                              explain-case))))
                 f))

;; --- Main entry point
(defn report [s p l n d]
  (call (outside-in->> binding-test-output|
                       with-status-in-context|
                       (on| [:after :block] remove-effect-data)
                       separating-levels|
                       doing-reports-at-report-level|
                       ; explaining-reports-at-explain-level|
                       handling-stats-at-stats-level|
                       handling-silent-mode|
                       handling-dots-mode|
                       base-report)
        s p l n d))
