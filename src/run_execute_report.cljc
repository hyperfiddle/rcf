
(def ^:private ^:dynamic *testing-state* nil)

(defn- fail-fast! [state ns rpt]
  ;; since the ex we are about to throw will be printed on *err*, and to avoid
  ;; a printing race condition with *out* when it's bound to it, we need to
  ;; force the past tests to actually print to the screen before throwing the ex
  ;; for this one.
  (flush)
  (throw (ex-info
           (str "Test " (name (:status rpt)) ":\n"
                (with-config {:out *out*}
                  (let [report-fn (-> (config) :report-fn)]
                    (with-out-str
                      (report-fn state :after :case ns rpt)))))
           (assoc rpt :type :minitest/fail-fast))))

(macros/deftime
  (defmacro ^:private ensuring-testing-state [& body]
    `(binding [*testing-state* (or *testing-state* (atom {}))]
       ~@body))

  (defmacro ^:private let-testing-fns [conf & body]
    `(let [{~'&run :run-fn ~'&execute :execute-fn ~'&report :report-fn} ~conf]
       ~@body))

  (defmacro ^:private doseq-each-lang [conf & body]
    `(let [conf# ~conf]
       (doseq [l# (:langs conf#)]
         (let-testing-fns conf#
           ~@body))))

  (defmacro ^:private for-each-lang [conf & body]
    `(->> (let [conf# ~conf]
            (for [l# (:langs conf#)]
              (let-testing-fns conf#
                [l# (do ~@body)])))
          (into {}))))

(defn orchestrate-level [state level ns data
                         & {:keys [handle-before handle-after]}]
  (let-testing-fns (config)
    (let [handle-before (or handle-before (fn [s l n d]
                                            (&report  s :before l n d)
                                            (&execute s :before l n d)))
          handle-after  (or handle-after  (fn [s l n d]
                                            (&execute s :after l n d)
                                            (&report  s :after l n d)))]
      (handle-before       state level ns data)
      (let [rpt-data (&run state level ns data     &execute)]
        (handle-after      state level ns rpt-data)))))

(defn with-test-level|    [f] (fn [s l n d]
                                (with-context {:test-level l}
                                 (f s l n d))))
(defn with-lang|          [f] (fn [s l n d]
                                (with-context {:lang :clj}
                                 (f s l n d))))
(defn with-ns-in-context| [f] (fn [s l n d]
                                (if-not (= l :ns)
                                 (f s l n d)
                                 (with-context {:ns n}
                                   (f s l n d)))))
(defn handling-fail-fast| [f] (fn [s l n d]
                                (if-not (= l :case)
                                 (f s l n d)
                                 (orchestrate-level
                                   s l n d
                                   :handle-after
                                   (fn [s l n d]
                                     (let [conf (config)]
                                       (let-testing-fns conf
                                         (if (and (:fail-fast conf)
                                                  (some-> d :status
                                                          #{:error :failure}))
                                           (fail-fast! s n d)
                                           (do
                                             (&execute s :after l n d)
                                             (&report  s :after l n d))))))))))

(def orchestrate
  (outside-in->> (with-test-level|)
                 (with-lang|)
                 (with-ns-in-context|)
                 (handling-fail-fast|)
                 orchestrate-level))

(defn ^:no-doc run-execute-report!
  ([level ns->tsts]
   (run-execute-report! level nil ns->tsts))
  ([level ns data]
   (let [orchestrate-fn (-> (config) :orchestrate-fn)]
     (ensuring-testing-state
       (orchestrate-fn *testing-state* level ns data)))))
