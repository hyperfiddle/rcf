;; Adapted from `https://github.com/clojure/core.unify` which was not cljs
;; compatible out of the box.

(ns hyperfiddle.rcf.unify
  (:require [clojure.walk :as walk]
            [clojure.set :as set]))

(defn wildcard? [x] (= '_ x))

(defn &? [form] (and (seqable? form) (= '& (first form))))

(defn lvar? [x] (and (symbol? x) (= \? (first (name x)))))

(defn failed? [env] (contains? env ::fail))

(defn unify-in-env [x y env]
  (if (contains? env x)
    (let [y' (get env x)]
      (if (= y y')
        env
        (if (lvar? y')
          (unify-in-env y' y env)
          (assoc env ::fail {x [y' y]}))))
    (assoc env x y)))

(defn wildcard-in-env [v env]
  (if (contains? env '_)
    (update-in env ['_] conj v)
    (assoc env '_ [v])))

(defn resolve*
  ([env k]
   (resolve* [] env k))
  ([path env k]
   (if (= k (first path))
     ::cycle
     (let [v (get env k)]
       (if (lvar? v)
         (resolve* (conj path k) env v)
         v)))))

(defn ground [env]
  (if (map? env)
    (let [env (reduce-kv (fn [env k _v] (assoc env k (resolve* env k))) env env)]
      (if (contains? env '_)
        (update env '_ (fn [xs] (mapv (fn [x] (if (lvar? x) (get env x) x)) xs)))
        env))
    env))

(declare unify)

(defn unify-set [xs ys env]
  (if (seq (set/intersection xs ys))
    (unify-set (set/difference xs ys) (set/difference ys xs) env)
    (let [env (unify (first xs) (first ys) env)]
      (if (failed? env)
        env
        (unify (rest xs) (rest ys) env)))))

(defn replace-keys [m ks-map]
  (reduce-kv (fn [r k v]
               (-> (dissoc r k)
                   (assoc (if (= k '_)
                            (or (first (set/difference (set (get ks-map '_)) (set (keys r))))
                                '_)
                            (get ks-map k k))
                          v)))
             m m))

(defn unify-map [xs ys env]
  (let [env (unify-set (set (keys xs)) (set (keys ys)) env)
        xs  (replace-keys xs env)
        ys  (replace-keys ys env)]
    (if (= xs ys)
      env
      (reduce (fn [env k]
                (let [env (unify (find xs k) (find ys k) env)]
                  (if (failed? env)
                    (reduced env)
                    env)))
              env (set/union (set (keys xs)) (set (keys ys)))))))

;; Javascript do not have chars. So iterating a string always produce more strings -> StackOverflow.
(defn collection? [x] (and (seqable? x) (not (string? x))))

(defn unify
  ([x y] (unify x y {}))
  ([x y env]
   (let [env (cond
               (failed? env)              env
               (wildcard? x)              (wildcard-in-env y env)
               (wildcard? y)              (wildcard-in-env x env)
               (= x y)                    env
               (&? x)                     (if (seq y)
                                            (unify (second x) (seq y) env)
                                            env)
               (&? y)                     (if (seq x)
                                            (unify (second y) (seq x) env)
                                            env)
               (lvar? x)                  (unify-in-env x y env)
               (lvar? y)                  (unify-in-env y x env)
               (and (set? x) (set y))     (unify-set x y env)
               (and (map? x) (map? y))    (unify-map x y env)
               (every? collection? [x y]) (let [env (unify (first x) (first y) env)]
                                            (if (failed? env)
                                              env
                                              (unify (rest x) (rest y) env)))
               :else                      (assoc env ::fail {::root [x y]}))]
     (if (failed? env)
       (update env ::path (fnil conj ()) x)
       env))))

(defn subst [form env]
  (let [idx      (volatile! -1)
        get-idx! (fn [] (vswap! idx inc))]
    (if (map? env)
      (walk/prewalk (fn [expr] (cond
                                 (lvar? expr)     (get env expr expr)
                                 (wildcard? expr) (get-in env ['_ (get-idx!)] '_)
                                 :else            expr))
                    form)
      form)))

(defn unifier* [x y]
  (let [env (unify x y)]
    (if (failed? env)
      [::fail env]
      (let [env (ground env)]
        [(subst y env) env]))))

(def unifier (comp first unifier*))

(defn explain [env]
  (when-let [fail (::fail env)]
    (str "Failed to unify "
         (if-some [[a b] (::root fail)]
           (str (pr-str a) " and " (pr-str b))
           (let [[lvar [a b]] (first fail)]
             (str (pr-str lvar) " with " (pr-str a) " and " (pr-str b))))
         (when-some [path (seq (->> (::path env) (filter map-entry?) (map key)))]
           (str " in " (into [] path))))))