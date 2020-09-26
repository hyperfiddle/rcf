(require '[clojure.repl   :refer [pst]]
         '[clojure.pprint :refer [pprint]])

(defprotocol MinitestReportP
  (before-all  [this ns->tests])
  (before-ns   [this ns-name tests])
  (before-each [this ns-name test])
  (after-each  [this ns-name report])
  (after-ns    [this ns-name reports])
  (after-all   [this ns->reports]))

(defn- lines [s]
  (str/split s #"\r?\n"))

(defn- tabulate [s pad]
  (->> (lines s)
       (map #(str pad %))
       (str/join "\n")))

(def ^:private term-width 80)

(defn- print-result [report logo left-ks right-ks]
  (let [left  (get-in report left-ks)
        right (get-in report right-ks)
        s     (format "%s %s => %s\n" logo left right)]
    (if (> (count s) (* 2 term-width))
      (let [left-pp  (with-out-str (pprint left))
            right-pp (with-out-str (pprint right))]
        (printf "%s %s\n" logo (first (lines left-pp)))
        (print (tabulate (apply str (rest (lines left-pp))) "  "))
        (println "=>")
        (print (tabulate right-pp)))
      (print s))))

(defn pluralize-on [x n]
  (str x (if (> n 1) "s" "")))

(defrecord SimpleReporter [opts store]
  MinitestReportP
  (before-all
    [this ns->tests]
    (reset! store {})
    (newline)
    (let [ns-cnt (count ns->tests)
          ts-cnt (->> ns->tests vals (apply concat) count)]
      (printf "-- Running Minitest on %d %s (%d %s)\n"
                ns-cnt
                (-> "namespace"  (pluralize-on ns-cnt))
                ts-cnt
                (-> "test"       (pluralize-on ts-cnt)))))
  (before-ns
    [this ns-name tests]
    (newline)
    (let [ts-cnt (count tests)]
      (printf "---- Testing %s (%d %s)\n"
              ns-name ts-cnt (-> "test" (pluralize-on ts-cnt)))))
  (before-each [this ns-name test])
  (after-each
    [this ns-name report]
    (case (:status report)
      :exception (do (print-result report (-> opts :exception :logo)
                                   [:result :form] [:expected :val])
                     (pst (:exception report) (:exception-depth opts))
                     (swap! store update-in [:counts :error] (fnil inc 0)))
      :failure   (do (print-result report (-> opts :failure :logo)
                                   [:result :form] [:expected :val])
                     (let [v (get-in report [:result :val])
                           s (with-out-str (println "Actual:" v))]
                       (if (> (count s) (* 2 term-width))
                         (do (println "Actual:" v)
                             (print (tabulate (with-out-str (pprint v)))))
                         (print s)))
                     (swap! store update-in [:counts :failure] (fnil inc 0)))
      :success   (if (:dots opts)
                   (print \.)
                   (print-result report (-> opts :success :logo)
                                 [:result :form] [:expected :val]))))
  (after-ns [this ns-name reports])
  (after-all
    [this ns->reports]
    (let [counts (:counts @store)]
      (when-not (-> (config) :fail-early)
        (newline)
        (printf "%d failures, %d errors.\n" (:failure counts) (:error counts))
        (newline)))))
