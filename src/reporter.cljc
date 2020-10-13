(require '[clojure.repl   :refer [pst]]
#?(:clj  '[clojure.pprint :refer [pprint]]
   :cljs '[cljs.pprint :refer [pprint]]))

(defprotocol ReporterP
  (before-report-suite     [this ns->tests])
  (before-report-namespace [this ns-name tests])
  (before-report-block     [this ns-name tests])
  (before-report-case      [this ns-name case])
  (report-case             [this ns-name report])
  (after-report-block      [this ns-name reports])
  (after-report-namespace  [this ns-name reports])
  (after-report-suite      [this ns->reports]))

(defn- lines   [s]         (str/split s #"\r?\n"))
(defn- tabl   ([s]         (tabl s "  "))
              ([s pad]     (->> (lines s)
                    (map #(str pad %))
                    (str/join "\n"))))
(defn- printab [tab s] (let [ls (lines s)]
                               (print tab (first ls))
                               (print (tabl (apply str (rest ls))
                                            (->> (repeat (count tab) \space)
                                                 (apply str))))))

(defn- ugly?
  ([s]            (ugly? s 1))
  ([s term-ratio] (> (count s)
                     (->> (config) :reporter :term-width (* term-ratio)))))

(defn- print-result [report logo left-ks right-ks]
  (let [left  (get-in report left-ks)
        right (get-in report right-ks)
        left-pp  (with-out-str (pprint left))
                    right-pp (with-out-str (pprint right))
        s     (print-str logo left "=>" right \newline)]
    ;; TODO
    (if-not (ugly? s)
      (print s)
      (do
        (printab logo left-pp)
        (println "=>")
        (printab (apply  str  (repeat (count (str logo)) \space))  right-pp)))))

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
    (newline)
    (let [ns-cnt (count ns->tests)
          ts-cnt (->> ns->tests vals (apply concat) (apply concat) count)]
      (once store [:announced-suite]
            (printf "-- Running minitest on %d %s (%d %s)\n"
                    ns-cnt (-> "namespace" (pluralize-on ns-cnt))
                    ts-cnt (-> "test"      (pluralize-on ts-cnt))))))
  (before-report-namespace
    [this ns-name tests]
    (let [ts-cnt (->> tests (apply concat) count)]
      (printf (if-once store [:announced-nss ns-name]
                "---- Testing %s (%d %s)\n"
                "---- Testing %s (%d more %s)\n")
              ns-name ts-cnt (-> "test" (pluralize-on ts-cnt)))))
  (before-report-block [this ns-name tests] nil)
  (before-report-case  [this ns-name test]  nil)
  (report-case
    [this ns-name report]
    (let [status (:status report)
          conf     (with-contexts {:status status} (config))
          logo     (-> conf :reporter :logo)
          left-ks  [:result :form]
          right-ks [:expected :val]
          left     (get-in report left-ks)]
      (with-contexts {:status status}
        (if (-> conf :reporter :dots)
          (print logo)
          (do (swap! store update-in [:counts status] (fnil inc 0))
              (print-result report logo left-ks right-ks)
              (case status
                :error   (pst (:error report) (:error-depth opts))
                :failure (let [v      (-> report :result :val)
                               left-n (count (print-str logo left))
                               act    (tabl "Actual:" "   ")  ; 💪
                               act-n  (count act)             ; ✌️
                               pad    (-> (max (- left-n (- act-n 4)) 0)
                                          (repeat \space)
                                          (->> (apply str)))
                               prompt (str act pad)
                               s      (print-str prompt v "\n")]
                           (if-not (ugly? s)
                             (print s)
                             (printab prompt (with-out-str (pprint v)) "\n")))
                nil)))))
    report)
  (after-report-block
    [this ns-name reports]
    reports)
  (after-report-namespace
    [this ns-name reports]
    (newline)
    ;; If the last report was printed in `:dots` mode, it needs a newline
    (when (with-contexts {:status (-> reports last :status)}
            (-> (config) :reporter :dots))
      (newline))
    reports)
  (after-report-suite
    [this ns->reports]
    (let [counts (:counts @store)]
      (when-not (-> (config) :fail-early)
        (newline)
        (printf "%d failures, %d errors.\n"
                (:failure counts 0) (:error counts 0))
        (newline)))
    ns->reports))
