
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
  (str x (if (> n 1) "s" "")))

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
        ts-cnt    (->> ns->tests vals (apply concat) (apply concat) count)]
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
        ts-cnt (->> tests (apply concat) count)]
    (println "---- Testing" ns
             (str "(" ts-cnt \space
                  ;; TODO: replace with once.
                  (once-else state [:announced-nss ns] "more ")
                  (-> "test" (pluralize ts-cnt))
                  ")")))
  data)

(defn for-each-datum| [f]
  (fn [state position level ns data]
    (doall (for [datum data]
             (f state position level ns datum)))))

(defn report-case [state position level ns data]
  (if (= data :minitest/effect-performed)
    data
    (let [report   data
          status   (:status report)
          conf     (with-context {:status status} (config))
          logo     (-> conf :logo)
          left-ks  [:tested :form]
          right-ks [:expected :val]
          left     (get-in report left-ks)]
      (print-result report logo left-ks right-ks)
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
      (assoc data :reported true))))

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
  (outside-in->> (on| [:before :suite]  announce-suite)
                 (on| [:before :ns]     announce-ns)
                 (on| [:after  :case]   #((:report-fn (config))
                                          %1 :do :case %4 %5))
                 (on| [:do     :case]   (when-not-reported| report-case))
                 (on| [:after  :block]  remove-effect-data)))
;; --- STATS
(defn increment-stats [state position level ns data]
  (let [report data]
    (swap! state update-in [:counts (:status report)]
           (fnil inc 0)))
  data)

(defn display-stats [state position level ns data]
  (let [counts (:counts @state)]
    (when-not (-> (config) :fail-fast)
      (newline)
      (println (as-> (:failure counts 0) $
                 (str $ " " (pluralize "failure" $) ", "))
               (as-> (:error   counts 0) $
                 (str $ " " (pluralize "error"   $) ".")))
      (newline)))
  data)

(defn handling-stats| [f]
  (outside-in->> (on| [:do     :case]   increment-stats)
                 (on| [:after  :suite]  display-stats)
                 f))

;; --- DOTS MODE
(defn report-case-as-dot [state position level ns data]
  (with-context {:status (:status data)}
    (print (-> (config) :logo)))
  data)

(defn handling-dots-mode| [f]
  (outside-in->> (on| [:do     :case]   (on-config| {:dots true}
                                          (when-not-reported|
                                            (marking-as-reported|
                                              report-case-as-dot))))
                 f))

;; --- SILENT MODE
(defn do-nothing [state position level ns data]
  data)

(defn handling-silent-mode| [f]
  (let [silence (on-config| {:silent true}
                  (when-not-reported|
                    (marking-as-reported|
                      do-nothing)))]
    (outside-in->> (on| [:pre  :case]  silence)
                   (on| [:do   :case]  silence)
                   (on| [:post :case]  silence)
                   f)))

(defn continue-when| [pred continue]
  (fn [& args]
    (if (apply pred args)
      (apply continue args)
      (last args)))) ;; last args is data

(defn without-config| [ks f]
  (fn [& args]
    (without-config ks
      (apply f args))))

(def ^:private ^:dynamic *blocking-reports-below* true)
(def ^:private ^:dynamic *blocking-redirection*   false)
(defn doing-reports-at-level| [level f]
  (let [no-op       (fn  ([s l n d]  d)  ([s p l n d]  d))
        run-reports (fn [s p l n d]
                      (if *blocking-redirection*
                        d
                        (binding [*blocking-reports-below* false
                                  *blocking-redirection*   true]
                          (with-context {:run-fn     no-op
                                         :execute-fn no-op}
                            ((-> (config) :run-fn)  s l n d)))))
        level-below {:suite :ns
                     :ns    :block
                     :block :case}]
    (assert (level-below level) (str "No level below " level))
    (outside-in->> (continue-when|      (fn [s p l n d]
                                          (not (and (= l (level-below level))
                                                    *blocking-reports-below*))))
                   (on| [:after level]  run-reports)
                   f)))

(def report
  (outside-in->> binding-test-output|
                 with-status-in-context|
                 (on| [:after :block] remove-effect-data)
                 (doing-reports-at-level| :block)
                 separating-levels|
                 handling-stats|
                 handling-silent-mode|
                 handling-dots-mode|
                 base-report))
