
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

(defn- runner    [conf] (construct-record conf [:runner]))
(defn- reporter  [conf] (construct-record conf [:reporter]))
(defn- executors [conf] (->> (for [k (:langs conf)]
                               [k (construct-record conf [:executor k])])
                             (into {})))

(def ^:dynamic *runner*)
(def ^:dynamic *reporter*)
(def ^:dynamic *executors*)

(defmacro ^:private ensuring-runner+executors+reporter [& body]
  `(let [c# (config)
         b?# bound?]
     (binding
       [*runner*    (if-not (b?# #'*runner*)    (runner    c#) *runner*)
        *reporter*  (if-not (b?# #'*reporter*)  (reporter  c#) *reporter*)
        *executors* (if-not (b?# #'*executors*) (executors c#) *executors*)]
       ~@body)))

(defn- fail-fast! [ns rpt]
  ;; since the ex we are about to throw will be printed on *err*, and to avoid
  ;; a printing race condition with *out*, we need to force the past tests to
  ;; actually print to the screen before throwing the ex for this one.
  (flush)
  (throw (ex-info
           (format "Test %s:\n%s"
                   (name (:status rpt))
                   (with-out-str
                     (with-config {:reporter {:out *out*}}
                       (report-case *reporter* ns rpt))))
           (assoc rpt :type :minitest/fail-fast))))

(defmacro ^:private doseq-each-executor [conf & body]
  `(doseq [[~'&executor-name ~'&executor] (executors ~conf)]
     ~@body))
(defmacro ^:private for-each-executor [conf expr]
  `(for   [[~'&executor-name ~'&executor] (executors ~conf)]
     ~expr))

(defn ^:no-doc run-execute-report!
  ([test-level ns->tsts]
   (run-execute-report! test-level nil ns->tsts))
  ([test-level ns tsts]
   (with-contexts {:test-level test-level} ;; TODO: exploit
     (let [conf (config)]
       (ensuring-runner+executors+reporter
         (case test-level
           :suite     (do (before-report-suite          *reporter* tsts)
                          (doseq-each-executor   conf
                            (before-execute-suite       &executor  tsts))
                          (let [ns->rpts (run-suite     *runner*   tsts)]
                            (doseq-each-executor conf
                              (after-execute-suite      &executor  ns->rpts))
                            (after-report-suite         *reporter* ns->rpts)))

           :namespace (do (before-report-namespace      *reporter* ns  tsts)
                          (doseq-each-executor   conf
                            (before-execute-namespace   &executor  ns  tsts))
                          (let [rpts     (run-namespace *runner*   ns  tsts)]
                            (doseq-each-executor conf
                              (after-execute-namespace  &executor  ns  rpts))
                            (after-report-namespace     *reporter* ns  rpts)))

           :block     (do (before-report-block          *reporter* ns  tsts)
                          (doseq-each-executor   conf
                            (before-execute-block       &executor  ns  tsts))
                          (let [rpts (doseq-each-executor conf
                                       (->>
                                         (run-block     *runner*   ns  tsts)
                                         (remove #{:minitest/effect-performed}))
                                       )]
                            (doseq-each-executor conf
                              (after-execute-block      &executor  ns  rpts))
                            (after-report-block         *reporter* ns  rpts)))

           :case      (let [tst tsts]
                        (before-report-case             *reporter* ns  tst)
                        (doseq-each-executor     conf
                          (before-execute-case          &executor  ns  tst))
                        (let [conf     (config)
                              exe->rpt (->>
                                         (for-each-executor conf
                                           [&executor-name
                                             (run-case  *runner*
                                                        &executor  ns  tst)])
                                         (into {}))
                              status   (:status exe->rpt)]
                          (pprint exe->rpt)
                          (if (= exe->rpt :minitest/effect-performed)
                            (doseq-each-executor conf
                              (after-execute-case       &executor ns  exe->rpt))
                            (if (and (:fail-fast conf)
                                     (-> status #{:error :failure}))
                              (fail-fast!                           ns  exe->rpt)
                              (report-case              *reporter*  ns  exe->rpt)))))))))))
