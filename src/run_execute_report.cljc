
(defn- construct-record [conf ks]
  (let [c (get-in conf (conj ks :class))]
    (-> (.getCanonicalName c)
        ;; replace the last occurence of "." with "/map->" to find
        ;; the fully-qualified name of the constructor fn
        (str/replace #"\.(?=[^.]+$)" "/map->") ;; (?=...) is a regex lookahead
        (str/replace \_ \-) ;; TODO: other chars to replace ?
        symbol resolve deref
        (call {:opts  (get-in conf ks)
               :store (atom {})}))))

(def ^:private runner   #(construct-record % [:runner]))
(def ^:private executor #(construct-record % [:executor :clj]))
(def ^:private reporter #(construct-record % [:reporter]))

(def ^:dynamic *runner*)
(def ^:dynamic *executor*)
(def ^:dynamic *reporter*)

(defmacro ^:private ensuring-runner+executor+reporter [& body]
  `(let [c# (config)
         b?# bound?]
     (binding [*runner*   (if-not (b?# #'*runner*)   (runner   c#) *runner*)
               *executor* (if-not (b?# #'*executor*) (executor c#) *executor*)
               *reporter* (if-not (b?# #'*reporter*) (reporter c#) *reporter*)]
       ~@body)))

(defn ^:no-doc run-execute-report!
  ([mode ns->tsts] (run-execute-report! mode nil ns->tsts))
  ([mode ns tsts]
   (let [exe (executor (config))]
     (ensuring-runner+executor+reporter
        (case mode
          :suite     (do (before-report-suite          *reporter* tsts)
                         (before-execute-suite         *executor* tsts)
                         (let [ns->rpts (run-suite     *runner*   tsts)]
                           (after-execute-suite        *executor* ns->rpts)
                           (after-report-suite         *reporter* ns->rpts)))
          :namespace (do (before-report-namespace      *reporter* ns  tsts)
                         (before-execute-namespace     *executor* ns  tsts)
                         (let [rpts     (run-namespace *runner*   ns  tsts)]
                           (after-execute-namespace    *executor* ns  rpts)
                           (after-report-namespace     *reporter* ns  rpts)))
          :block     (do (before-report-block          *reporter* ns  tsts)
                         (before-execute-block         *executor* ns  tsts)
                         (let [rpts     (run-block     *runner*   ns  tsts)]
                           (after-execute-block        *executor* ns  rpts)
                           (after-report-block         *reporter* ns  rpts)))
          :case      (let [tst tsts]
                       (before-report-case             *reporter* ns  tst)
                       (before-execute-case            *executor* ns  tst)
                       (let [conf (config)
                             exe (executor conf)
                             rpt        (run-case      *runner*   exe ns tsts)
                             status     (:status rpt)]
                         (after-execute-case           *executor* ns  rpt)
                         (if (and (:fail-fast conf)
                                  (-> status #{:error :failure}))
                           (do (flush) ;; since the ex will be printed on *err*
                               (throw (ex-info
                                        (format
                                          "Test %s:\n%s"
                                          (name status)
                                          (with-out-str
                                            (with-config {:reporter {:out *out*}}
                                              (report-case *reporter* ns rpt))))
                                        (assoc rpt :type :minitest/fail-fast))))
                           (report-case                *reporter* ns rpt)))))))))
