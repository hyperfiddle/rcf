
(defn- construct-record [conf k assert-proto]
  (let [c (get-in conf [k :class])]
    (-> (.getCanonicalName c)
        ;; replace the last occurence of "." with "/map->" to find
        ;; the fully-qualified name of the constructor fn
        (str/replace #"\.(?=[^.]+$)" "/map->") ;; (?=...) is a regex lookahead
        (str/replace \_ \-) ;; TODO: other chars to replace ?
        symbol resolve deref
        (call {:opts  (get conf k)
               :store (atom {})}))))

(def ^:private runner   #(construct-record % :runner   RunnerP))
(def ^:private reporter #(construct-record % :reporter ReporterP))
(def ^:dynamic *runner*)
(def ^:dynamic *reporter*)

(defmacro ^:private ensuring-runner&reporter [& body]
  `(let [c# (config)
         b?# bound?]
     (binding [*runner*   (if-not (b?# #'*runner*)   (runner   c#) *runner*)
               *reporter* (if-not (b?# #'*reporter*) (reporter c#) *reporter*)]
       ~@body)))

(defn ^:no-doc run-and-report!
  ([mode ns->tsts] (run-and-report! mode nil ns->tsts))
  ([mode ns tsts]
   (ensuring-runner&reporter
     (case mode
       :suite     (do (before-suite *reporter* tsts)
                      (let [ns->rpts (run-suite *runner* tsts)]
                        (after-suite *reporter* ns->rpts)))
       :namespace (do (before-namespace *reporter* ns tsts)
                      (let [rpts (run-namespace *runner* ns tsts)]
                        (after-namespace *reporter* ns rpts)))
       :block     (do (before-block *reporter* ns tsts)
                      (let [rpts (run-block *runner* ns tsts)]
                        (after-block *reporter* ns rpts)))
       :case      (do (before-case *reporter* ns tsts)
                      (let [rpt  (run-case *runner* ns tsts)]
                        (after-case *reporter* ns rpt)))))))
