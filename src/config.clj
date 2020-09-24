(require '[clojure.java.io :as io]
         '[clojure.edn     :as edn])

(declare config)
(def ^:dynamic *config*)

(def default-config
  "Any config you may provide to minitest will merge into this base
  configuration map.

  See `(source default-config)`."
  {:fail-early       false
   :silent-success   false
   :break-on-failure false ; TODO
   :formatter        :simple
   :load-tests       true
   :on-load {:store  true
             :run    false}
   :on-eval {:store  false
             :run    true}
   :profiles {:production  {:load-tests       false}
              :development {:break-on-failure true}}})

(defn- read-config []
  (let [f (io/file "./minitest.edn")]
    (-> (or (when (.exists f) f)
            (io/resource "minitest.edn"))
        (some-> slurp edn/read-string))))

(def ^:dynamic *config* (read-config))

;; Taken from https://gist.github.com/danielpcox/c70a8aa2c36766200a95
(defn- deep-merge [& maps]
  (apply merge-with (fn [& args]
                      (if (every? #(or (map? %) (nil? %)) args)
                        (apply deep-merge args)
                        (last args)))
         maps))

(defn- profile-config [profile]
  (let [profiles (:profiles *config*)
        raw-profile (get profiles profile)]
    (->> (if (sequential? raw-profile)
           raw-profile
           [raw-profile])
         (map #(cond (map? %)        %
                     (sequential? %) (profile-config %)
                     :else           (get profiles %)))
         (apply deep-merge))))

(defn config
  ([] (config *profile*))
  ([profile]
   (deep-merge default-config
               (dissoc *config* :profiles)
               (profile-config profile))))
