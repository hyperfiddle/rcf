(require '[clojure.repl   :refer [pst]]
         '[clojure.pprint :refer [pprint]])

(defprotocol ReporterP
  (before-suite     [this ns->tests])
  (before-namespace [this ns-name tests])
  (before-block     [this ns-name tests])
  (before-case      [this ns-name case])
  (after-case       [this ns-name case])
  (after-block      [this ns-name report])
  (after-namespace  [this ns-name reports])
  (after-suite      [this ns->reports]))

(defn- lines     [s]         (str/split s #"\r?\n"))
(defn- tabulate ([s]         (tabulate s "  "))
                ([s pad]     (->> (lines s)
                                  (map #(str pad %))
                                  (str/join "\n"))))
(defn- printab   [incipit s] (let [ls (lines s)]
                               (print incipit (first ls))
                               (print (tabulate
                                        (apply str (rest ls))
                                        (->> (repeat (count incipit) \space)
                                             (apply str))))))

(defmacro ^:private print-maybe-pretty [s & body]
  `(let [s# ~s]
     (if (> (count s#) (-> (config) :pretty-limit))
       (do ~@body)
       (print s#))))

(defn- print-result [report logo left-ks right-ks]
  (let [left  (get-in report left-ks)
        right (get-in report right-ks)
        s     (format "%s %s => %s\n" logo left right)]
    (print-maybe-pretty
      (format "%s %s => %s\n" logo left right)
      (let [left-pp  (with-out-str (pprint left))
            right-pp (with-out-str (pprint right))]
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
  (before-suite
    [this ns->tests]
    (newline)
    (let [ns-cnt (count ns->tests)
          ts-cnt (->> ns->tests vals (apply concat) (apply concat) count)]
      (once store [:announced-suite]
            (printf "-- Running minitest on %d %s (%d %s)\n"
                    ns-cnt (-> "namespace" (pluralize-on ns-cnt))
                    ts-cnt (-> "test"      (pluralize-on ts-cnt))))))
  (before-namespace
    [this ns-name tests]
    (let [ts-cnt (->> tests (apply concat) count)]
      (printf (if-once store [:announced-nss ns-name]
                "---- Testing %s (%d %s)\n"
                "---- Testing %s (%d more %s)\n")
              ns-name ts-cnt (-> "test" (pluralize-on ts-cnt)))))
  (before-block [this ns-name tests] nil)
  (before-case  [this ns-name test]  nil)
  (after-case
    [this ns-name report]
    (let [status (:status report)
          conf   (with-contexts {:status status} (config))
          logo   (-> conf :reporter :logo)]
      (with-contexts {:status status}
        (if (-> conf :reporter :dots)
          (print logo)
          (do (swap! store update-in [:counts status] (fnil inc 0))
              (print-result report logo [:result :form] [:expected :val])
              (case status
                :error   (pst (:error report) (:error-depth opts))
                :failure (let [v   (-> report :result :val)
                               act "  Actual:"
                               s   (with-out-str (println act v))]
                           (print-maybe-pretty
                             (print-str act v "\n")
                             (printab act (with-out-str (pprint v)) "\n")))
                nil)))))
    report)
  (after-block
    [this ns-name reports]
    reports)
  (after-namespace
    [this ns-name reports]
    (newline)
    ;; If the last report was printed in `:dots` mode, it needs a newline
    (when (with-contexts {:status (-> reports last :status)}
            (-> (config) :reporter :dots))
      (newline))
    reports)
  (after-suite
    [this ns->reports]
    (let [counts (:counts @store)]
      (when-not (-> (config) :fail-early)
        (newline)
        (printf "%d failures, %d errors.\n"
                (:failure counts 0) (:error counts 0))
        (newline)))
    ns->reports))
