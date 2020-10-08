
(declare default-config)

(defn- read-config []
  (let [f (io/file "./minitest.edn")]
    (-> (or (when (.exists f) f)
            (io/resource "minitest.edn"))
        (some-> slurp edn/read-string))))

(def ^:dynamic *config* (read-config))

;; Taken from https://gist.github.com/danielpcox/c70a8aa2c36766200a95
;; TODO: acknowledge.
(defn ^:no-doc deep-merge [& maps]
  (apply merge-with (fn [& args]
                      (if (every? #(or (map? %) (nil? %)) args)
                        (apply deep-merge args)
                        (last args)))
         maps))

(defn- parse-profile [m x]
  (condp call x
    map?        x
    sequential? (->> (map (partial parse-profile m) x)
                     (apply deep-merge))
    (do         (some-> m :contexts (get x) (->> (parse-profile m))))))

(defn context-map? [x]
  (and (map? x) (contains? x :contexts)))

(defn contextualize [ctxs m]
  (->> m (clojure.walk/postwalk
           (fn [form]
             (if-not (context-map? form)
               form
               (let [c           (:contexts form)
                     active-ctxs (select-keys ctxs (keys c))
                     c-ms        (map #(parse-profile form (get-in c %))
                                      active-ctxs)]
                 (-> (apply deep-merge form c-ms)
                     ;; prevent merging contexts from contexts
                     (assoc :contexts (:contexts form)))))))))

(defn config
  [& [profile]]
  (->> [default-config *config* profile]
       (map (partial contextualize *contexts*))
       (apply deep-merge)))

(defmacro with-config [m & body]
  `(binding [*config* (deep-merge *config* ~m)]
     ~@body))

(defmacro with-contexts [m & body]
  `(binding [*contexts* (merge *contexts* ~m)]
     ~@body))

