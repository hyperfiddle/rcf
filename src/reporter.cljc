(require '[clojure.repl   :refer [pst]]
#?(:clj  '[clojure.pprint :refer [pprint] :as pp]
   :cljs '[cljs.pprint    :refer [pprint] :as pp]))

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
                     (->> (config) :reporter :term-width (* term-ratio)))))

(defn pprint-str [x] (with-out-str (pprint x)))

(defn- print-result [report logo left-ks right-ks]
  (let [left     (get-in report left-ks)
        right    (get-in report right-ks)
        s        (str logo " " (pprint-str left) " => " (pprint-str  right))]
    ;; TODO
    (if-not (ugly? s)
      (print s)
      (do
        (printab logo (pprint-str left))
        (newline)
        (printab (str (apply  str  (repeat (+ 2 (count (str logo))) \space))
                      "=> ")
                 (pprint-str right))
        (newline)))))

(defmacro ^:private once [a pth expr]
  `(let [a# ~a  pth# ~pth]
     (when-not (get-in @a# pth#)
       (let [result# ~expr]
         (swap! a# assoc-in pth# true)
         result#))))

(defmacro ^:private if-once [a pth expr & [else]]
  `(if-let [cached# (once ~a ~pth ~expr)]
     cached#
     ~else))

(defn pluralize-on [x n]
  (str x (if (> n 1) "s" "")))

(defrecord TermReporter [opts store]
  ReporterP
  (before-report-suite
    [this ns->tests]
    (binding [*out* (-> (config) :reporter :out)]
      (newline)
      (let [ns-cnt (count ns->tests)
            ts-cnt (->> ns->tests vals (apply concat) (apply concat) count)]
        (once store [:announced-suite]
              (printf "-- Running minitest on %d %s (%d %s)\n"
                      ns-cnt (-> "namespace" (pluralize-on ns-cnt))
                      ts-cnt (-> "test"      (pluralize-on ts-cnt)))))))
  (before-report-namespace
    [this ns-name tests]
    (binding [*out* (-> (config) :reporter :out)]
      (let [ts-cnt (->> tests (apply concat) count)]
        (printf (if-once store [:announced-nss ns-name]
                  "---- Testing %s (%d %s)\n"
                  "---- Testing %s (%d more %s)\n")
                ns-name ts-cnt (-> "test" (pluralize-on ts-cnt))))))
  (before-report-block [this ns-name tests] nil)
  (before-report-case  [this ns-name test]  nil)
  (report-case
    [this ns-name report]
    (binding [*out* (-> (config) :reporter :out)]
      (let [status   (:status report)
            conf     (with-contexts {:status status} (config))
            logo     (-> conf :reporter :logo)
            left-ks  [:result :form]
            right-ks [:expected :val]
            left     (get-in report left-ks)]
        (swap! store update-in [:counts status] (fnil inc 0))
        (with-contexts {:status status}
          (when (#{:error :failure} status) (newline))
          (cond
            (-> conf :reporter :silent) nil
            (-> conf :reporter :dots)   (print logo)
            :else
            (do (print-result report logo left-ks right-ks)
                (case status
                  :success nil
                  :error   (binding [*err* *out*]
                             (pst (:error report) (:error-depth opts)))
                  :failure (let [v      (binding [*print-level* 10000
                                                  pp/*print-pprint-dispatch*
                                                  pp/code-dispatch]
                                          (-> report :result :val pprint-str))
                                 left-n (count (str logo " " (pprint-str left)))
                                 prompt (str "Actual: ")
                                 s      (str prompt v)]
                             (if-not (ugly? s)
                               (print s)
                               (do (printab prompt v)
                                   (newline))))))))
        (when (#{:error :failure} status) (newline)))
      report))
  (after-report-block
    [this ns-name reports]
    reports)
  (after-report-namespace
    [this ns-name reports]
    (binding [*out* (-> (config) :reporter :out)]
      (newline)
      ;; If the last report was printed in `:dots` mode, it needs a newline
      (when (with-contexts {:status (-> reports last :status)}
              (-> (config) :reporter :dots))
        (newline))
      reports))
  (after-report-suite
    [this ns->reports]
    (binding [*out* (-> (config) :reporter :out)]
      (let [counts (:counts @store)]
          (when-not (-> (config) :fail-fast)
            (newline)
            (printf "%d failures, %d errors.\n"
                    (:failure counts 0) (:error counts 0))
            (newline)))
        ns->reports)))
