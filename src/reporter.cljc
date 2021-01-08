
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


(defn report-in-terminal [state position level ns-name data]
  (binding [*out* (-> (config) :out)]
    (case [position level]
      [:before :suite]
      (let [ns->tests data
            ns-cnt    (count ns->tests)
            ts-cnt    (->> ns->tests vals (apply concat) (apply concat) count)]
        (newline)
        (println "ONCE STATE" state)
        (once
          state [:announced-suite]
          (println
            "-- Running minitest on"
            ns-cnt (-> "namespace" (pluralize ns-cnt))
            (str \( ts-cnt \space (-> "test" (pluralize ts-cnt)) \)))))

      [:before :ns]
      (let [tests  data
            ts-cnt (->> tests (apply concat) count)]
        (println "---- Testing" ns-name
                 (str "(" ts-cnt \space
                      (once-else state [:announced-nss ns-name] "more ")
                      (-> "test" (pluralize ts-cnt))
                      ")")))

      [:before :block]  nil
      [:before :case]   nil

      [:after  :case]
      (when (map? data)
        (let [report   data
              status   (:status report)
              conf     (with-context {:status status} (config))
              logo     (-> conf :logo)
              left-ks  [:tested :form]
              right-ks [:expected :val]
              left     (get-in report left-ks)]
          (swap! state update-in [:counts status]
                 (fnil inc 0))
          (with-context {:status status}
            (binding [*out* (-> conf :out)]
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
                                               (-> conf :error-depth)))
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
                                       (newline)))))))
              (flush))
            (when (#{:error :failure} status) (newline)))
          (flush)))

      [:after  :block]
      (remove #{:minitest/effect-performed} data)

      [:after  :ns]
      (do
        (newline)
        ;; If the last report was printed in `:dots` mode, it needs a newline
        (when (with-context {:status (-> data last :status)}
                (-> (config) :dots))
          (newline))
        data)

      [:after  :suite]
      (let [counts (:counts @state)]
        (when-not (-> (config) :fail-fast)
          (newline)
          (println (as-> (:failure counts 0) $
                     (str $ " " (pluralize "failure" $) ", "))
                   (as-> (:error   counts 0) $
                     (str $ " " (pluralize "error"   $) ".")))
          (newline))
        data))))
