;; Adapted from `https://github.com/clojure/core.unify` which was not cljs
;; compatible out of the box.

(ns hyperfiddle.rcf.unify
  (:require [clojure.walk :as walk]))

(defn wildcard? [x] (= '_ x))

(defn &? [form] (and (seqable? form) (= '& (first form))))

(defn lvar? [x] (and (symbol? x) (= \? (first (name x)))))

(defn unify-in-env [x y env]
  (if (contains? env x)
    (let [y' (get env x)]
      (if (= y y')
        env
        (if (lvar? y')
          (unify-in-env y' y env)
          (assoc env x ::fail))))
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
    (let [env (reduce-kv (fn [env k v] (assoc env k (resolve* env k))) env env)]
      (if (contains? env '_)
        (update env '_ (fn [xs] (mapv (fn [x] (if (lvar? x) (get env x) x)) xs)))
        env))
    env))

(defn unify
  ([x y] (unify x y {}))
  ([x y env]
   (cond
     (wildcard? x)           (wildcard-in-env y env)
     (wildcard? y)           (wildcard-in-env x env)
     (= x y)                 env
     (&? x)                  (if (seq y)
                               (unify (second x) (seq y) env)
                               env)
     (&? y)                  (if (seq x)
                               (unify (second y) (seq x) env)
                               env)
     (lvar? x)               (unify-in-env x y env)
     (lvar? y)               (unify-in-env y x env)
     (every? seqable? [x y]) (let [env (unify (first x) (first y) env)]
                               (if (= ::fail env)
                                 ::fail
                                 (unify (rest x) (rest y) env)))
     :else                   ::fail)))

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
    (if (= ::fail env)
      [::fail ::fail]
      (let [env (ground env)]
        [(subst y env) env]))))

(def unifier (comp first unifier*))
