
(defn- transpose [coll-of-colls]
  (apply map #(apply list %&) coll-of-colls))

(defn- coshaped? [[form & more-forms]]
  (every? (condp call form
            sequential?  sequential?
            associative? associative?
            coll?        coll?
            (constantly true))
          more-forms))

(defn- map-aligned [f m & ms]
  (map (fn [e]
         (let [k (key e)
               other-es (map #(find % k) ms)]
           (apply f e other-es)))
       m))

(defn- map+ [f & [form :as forms]]
  (letfn [(safe| [f]
                 (fn [& in-items]
                   (let [out-items (apply f in-items)]
                     (assert (= (count in-items) (count out-items)))
                     out-items)))]
    (assert (coshaped? forms)
            (apply str "Forms are not coshaped:\n"
                   (map #(str "- " \( (type %) \) \space % \newline)
                        forms)))
    (condp call form
      sequential?  (let [min-length (apply min (map count forms))]
                     (map #(concat %1 (drop min-length %2))
                          (transpose (apply map (safe| f) forms))
                          forms))
      associative? (let [ks        (map (comp set keys) forms)
                         all-ks    (apply set/union ks)
                         common-ks (apply set/intersection ks)
                         common-ms (map #(select-keys % common-ks) forms)
                         unique-ks (set/difference all-ks common-ks)
                         unique-ms (map #(select-keys % unique-ks) forms)]
                     (->> (apply map-aligned (safe| f) common-ms)
                          transpose
                          (map #(concat %2 %1) unique-ms))))))

;; TODO: handle records
(defn- comap [f & colls]
  (letfn [(reshape [original anew]
                   (condp call original
                     list?      (apply list anew)
                     map-entry? (vec anew)
                     seq?       (doall anew)
                     coll?      (into (empty original) anew)))]
    (->> (apply map+ f colls)
         (map reshape colls))))

(defn- cowalk [inner outer & [form :as forms]]
  {:test
   (fn []
     (is (= [[2 2 2] [1 1 1]] (cowalk (bounce| 0 +) #(do %&) [1 1 1] [1 1 1])))
     (is (= [[2 2 1] [1 1]]   (cowalk (bounce| 0 +) #(do %&) [1 1 1] [1 1])))
     (is (= [[1 1]   [2 2 1]] (cowalk (bounce| 1 +) #(do %&) [1 1] [1 1 1])))
     (is (= [{:a 2 :b 4} {:a 1 :b 2 :c 3}]
            (cowalk (bounce| 0 (fn [& [[k _v] :as es]]
                                 [k (apply + (map val es))]))
                    #(do %&)
                    {:a 1 :b 2}
                    {:a 1 :b 2 :c 3}))))}
  (if (every? coll? forms)
    (apply outer (apply comap inner forms))
    (apply outer forms)))

(defn- coprewalk [f & forms]
  {:test (fn []
           (is (= [[1 2 4] [identity identity inc]]
                  (coprewalk (bounce| 0 #(if  (number? %1)  (%2 %1)  %1))
                             [1 2 3]
                             [identity identity inc]))))}
  (apply cowalk (partial coprewalk f) #(do %&) (apply f forms)))

(defn- copostwalk [f & forms]
  {:test (fn []
           (is (= [[1 2 4] [identity identity inc]]
                  (copostwalk (bounce| 0 #(if  (number? %1)  (%2 %1)  %1))
                              [1 2 3]
                              [identity identity inc]))))}
  (apply cowalk (partial copostwalk f) f forms))
