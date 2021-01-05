
(defprotocol ReporterP
  (before-report-suite     [this ns->tests])
  (before-report-namespace [this ns-name tests])
  (before-report-block     [this ns-name tests])
  (before-report-case      [this ns-name case])
  (report-case             [this ns-name report])
  (after-report-block      [this ns-name reports])
  (after-report-namespace  [this ns-name reports])
  (after-report-suite      [this ns->reports]))

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
  (defmacro ^:private once [store pth expr]
    `(let [store# ~store  pth# ~pth]
       (when-not (get-in @store# pth#)
         (let [result# ~expr]
           (swap! store# assoc-in pth# ::run-once)
           result#))))

  ;; TODO: necessary ?
  (defmacro ^:private once-else [store pth expr & [else]]
    `(if-let [cached# (once ~store ~pth ~expr)]
       cached#
       ~else)))

(defn pluralize-on [x n]
  (str x (if (> n 1) "s" "")))

(defrecord TermReporter [opts store]
  ReporterP
  (before-report-suite
    [this ns->tests]
    (binding [*out* (-> (config) :out)]
      (newline)
      (let [ns-cnt (count ns->tests)
            ts-cnt (->> ns->tests vals (apply concat) (apply concat) count)]
        (once store [:announced-suite]
              (println
                "-- Running minitest on"
                ns-cnt (-> "namespace" (pluralize-on ns-cnt))
                (str \( ts-cnt \space (-> "test" (pluralize-on ts-cnt)) \)))))))
  (before-report-namespace
    [this ns-name tests]
    (binding [*out* (-> (config) :out)]
      (let [ts-cnt (->> tests (apply concat) count)]
        (println "---- Testing" ns-name
                 (str "(" ts-cnt \space
                      (once-else store [:announced-nss ns-name] "more ")
                      (-> "test" (pluralize-on ts-cnt))
                      ")")))))
  (before-report-block [this ns-name tests] nil)
  (before-report-case  [this ns-name test]  nil)
  (report-case
    [this ns-name report]
    (when (map? report)
      (binding [*out* (-> (config) :out)]
        (let [status   (:status report)
              conf     (with-context {:status status} (config))
              logo     (-> conf :logo)
              left-ks  [:tested :form]
              right-ks [:expected :val]
              left     (get-in report left-ks)]
          (swap! store update-in [:counts status] (fnil inc 0))
          (with-context {:status status}
            (when (#{:error :failure} status) (newline))
            (cond
              (-> conf :silent) nil
              (-> conf :dots)   (print logo)
              :else
              (do (print-result report logo left-ks right-ks)
                  (case status
                    :success nil
                    :error   #?(:clj  (binding [*err* *out*]
                                        ;; TODO: set error depth in cljs
                                        (pst (:error report)
                                             (:error-depth opts)))
                                :cljs (pst (:error report)))
                    :failure (let [v      (binding [*print-level* 10000
                                                    pp/*print-pprint-dispatch*
                                                    pp/code-dispatch]
                                            (-> report :tested :val pprint-str))
                                   left-n (count
                                            (str logo " " (pprint-str left)))
                                   prompt (str "Actual: ")
                                   s      (str prompt v)]
                               (if-not (ugly? s)
                                 (print s)
                                 (do (printab prompt v)
                                     (newline))))))))
          (when (#{:error :failure} status) (newline)))
        (flush)))
    report)
  (after-report-block
    [this ns-name reports]
    (remove #{:minitest/effect-performed} reports))
  (after-report-namespace
    [this ns-name reports]
    (binding [*out* (-> (config) :out)]
      (newline)
      ;; If the last report was printed in `:dots` mode, it needs a newline
      (when (with-context {:status (-> reports last :status)}
              (-> (config) :dots))
        (newline))
      reports))
  (after-report-suite
    [this ns->reports]
    (binding [*out* (-> (config) :out)]
      (let [counts (:counts @store)]
          (when-not (-> (config) :fail-fast)
            (newline)
            (println (:failure counts 0) "failures," (:error counts 0) "errors")
            (newline)))
        ns->reports)))
