(ns minitest.custom-map
  (:require [net.cgrand.macrovich           :as    macros])
  #?(:cljs
      (:require-macros [minitest.custom-map :refer [mcall]])))

(declare empty* get* assoc* dissoc* keys* with-meta* meta*)

(defn- map-entry-proxy [m k]
  (macros/case
    :clj  (reify
            clojure.lang.IMapEntry
            (key      [_] k)
            (val      [_] (.valAt m k))
            java.util.Map$Entry
            (getKey   [_] k)
            (getValue [_] (.valAt m k)))
    :cljs (reify
            IMapEntry
            (-key     [_] k)
            (-val     [_] (.valAt ^ILookup m k)))))

(deftype CustomMap [m type fns]
  #?(:clj clojure.lang.MapEquivalence)
  #?@(:clj  [clojure.lang.IPersistentCollection
             (equiv          [this o]     (and (or (map? o)
                                                   (instance? java.util.Map o))
                                               (= o (into {} this))))
             (cons           [this o]     (cond
                                            (map? o)
                                            (reduce #(apply assoc %1 %2) this o)

                                            (instance? java.util.Map o)
                                            (reduce #(apply assoc %1 %2)
                                                    this (into {} o))

                                            :else
                                            (if-let [[k v] (seq o)]
                                              (assoc this k v)
                                              this)))]
      :cljs [IEquiv
             (-equiv         [this o]     (and (map? o)
                                               (= o (into {} this))))
             ICollection
             (-conj          [this o]     (if (map? o)
                                            (reduce #(apply assoc %1 %2) this o)
                                            (if-let [[k v] (seq o)]
                                              (assoc this k v)
                                              this)))])
  #?@(:clj  [clojure.lang.IObj
             (withMeta       [this mta]   (with-meta* this mta))
             (meta           [this]       (meta* this))]
      :cljs [IWithMeta
             (-with-meta     [this mta]   (with-meta* this mta))
             IMeta
             (-meta          [this]       (meta* this))])

  Object
  #?@(:clj  [(hashCode       [this]       (reduce
                                            (fn [acc [k v]]
                                              (unchecked-add
                                                acc
                                                (bit-xor
                                                  (clojure.lang.Util/hash k)
                                                  (clojure.lang.Util/hash v))))
                                            0
                                            (seq this)))
             (equals         [this o]     (or (identical? this o)
                                              (.equiv this o)))]
      :cljs [(toString       [this]       (str (into {} this)))
             (equiv          [this o]     (-equiv this o))])

  #?@(:clj  [clojure.lang.ILookup
             (valAt          [this k]     (.valAt this k nil))
             (valAt          [this k dft] (get* this k dft))]
      :cljs [ILookup
             (-lookup        [this k]     (-lookup this k nil))
             (-lookup        [this k dft] (get* this k dft))])

  #?@(:clj  [clojure.lang.Associative
             (containsKey    [this k]     (contains? (.keySet this) k))
             (entryAt        [this k]     (when (contains? (.keySet this) k)
                                          (map-entry-proxy this k)))
             (assoc          [this k v]   (assoc* this k v))
             (empty          [this]       (empty* this))]
      :cljs [IAssociative
             (-contains-key? [this k]     (contains? (keys* this) k))
             (-assoc         [this k v]   (assoc* this k v))
             IEmptyableCollection
             (-empty         [this]       (empty* this))])
  #?@(:clj  [clojure.lang.IPersistentMap
             (assocEx        [this k v]   (if (contains? this k)
                                            (throw (Exception. "Key or value already present"))
                                            (assoc this k v)))
             (without        [this k]     (dissoc* this k))]
      :cljs [IMap
             (-dissoc        [this k]     (dissoc* this k))])

  #?@(:clj  [clojure.lang.Counted
             (count          [this]       (count (keys* this)))
             clojure.lang.Seqable
             (seq            [this]       (seq (map #(map-entry-proxy this %)
                                                             (keys* this))))
             java.util.Iterator
             (iterator       [t]          (clojure.lang.SeqIterator. t))
             clojure.lang.IFn
             (invoke         [this k]     (get* this k nil))
             (invoke         [this k dft] (get* this k dft))]
      :cljs [ICounted
             (-count         [this]       (count (keys* this)))
             ISeqable
             (-seq           [this]       (seq (map #(map-entry-proxy this %)
                                                    (keys* this))))
             IIterable
             (-iterator      [t]          (seq-iter t))
             IFn
             (-invoke        [this k]     (get* this k nil))
             (-invoke        [this k dft] (get* this k dft))])

  #?@(:clj  [clojure.core.protocols.CollReduce
             (coll-reduce    [this f]     (reduce f (seq this)))
             (coll-reduce    [this f val] (reduce f val (seq this)))
             clojure.lang.IHashEq
             (hasheq         [this]       (hash-unordered-coll (or (seq this)
                                                                   ())))
             java.util.Map
             (get         [this k]      (.valAt this k))
             (isEmpty     [this]        (empty? this))
             (size        [this]        (count this))
             (keySet      [this]        (set (keys* this)))
             (put         [_ _ _]       (throw (UnsupportedOperationException.)))
             (putAll      [_ _]         (throw (UnsupportedOperationException.)))
             (clear       [_]           (throw (UnsupportedOperationException.)))
             (remove      [_ _]         (throw (UnsupportedOperationException.)))
             (values      [this]        (->> this seq (map second)))
             (entrySet    [this]        (->> this seq set))
             java.util.concurrent.Callable
             (call        [this]        (.invoke this))
             java.lang.Runnable
             (run         [this]        (.invoke this))]))

(macros/deftime
  (defmacro mcall [obj f & args]
    (macros/case
      :clj  `(~(-> (str "."  f) symbol) ~obj ~@args)
      :cljs `(~(-> (str ".-" f) symbol) ~obj ~@args))))

(defn empty*     [^CustomMap m]       ((-> m (mcall fns) :empty)     m))
(defn get*       [^CustomMap m k dft] ((-> m (mcall fns) :get)       m k dft))
(defn assoc*     [^CustomMap m k v]   ((-> m (mcall fns) :assoc)     m k v))
(defn dissoc*    [^CustomMap m k]     ((-> m (mcall fns) :dissoc)    m k))
(defn keys*      [^CustomMap m]       ((-> m (mcall fns) :keys)      m))
(defn with-meta* [^CustomMap m mta]   ((-> m (mcall fns) :with-meta) m mta))
(defn meta*      [^CustomMap m]       ((-> m (mcall fns) :meta)      m))

(defn custom-map?
  ([m]      (instance? CustomMap m))
  ([m type] (and (instance? CustomMap m)
                 (= (mcall ^CustomMap m type) type))))

(defn func-map [m & {:keys [deep] :or {deep true}}]
  (let [anew #(CustomMap. %2 :func-map (mcall ^CustomMap %1 fns))]
    (CustomMap.
      m :func-map
      {:empty     (fn [^CustomMap this]
                    (empty (mcall this m)))
       :get       (fn [^CustomMap this k default]
                    (let [result (loop [result (get (mcall this m) k default)]
                                   (if (-> result meta :minitest/map-func)
                                     (recur (result this))
                                     result))]
                      (if (and (map? result)
                               (not (custom-map? result :func-map))
                               deep)
                        (anew this result)
                        result)))
       :assoc     (fn [^CustomMap this k v]  (anew this (assoc  (mcall this m) k v)))
       :dissoc    (fn [^CustomMap this k]    (anew this (dissoc (mcall this m) k)))
       :keys      (fn [^CustomMap this]      (keys (mcall this m)))
       :meta      (fn [^CustomMap this]      (meta (mcall this m)))
       :with-meta (fn [^CustomMap this meta] (anew this (with-meta (mcall this m) meta)))})))

(defn map-func| [f]
  (with-meta f {:minitest/map-func true}))

(macros/deftime
  (defmacro at-runtime [& body]
    `(map-func| (fn [~'&parent] ~@body))))

(defn filtered-map [m ks & {:keys [deep] :or {deep true}}]
  (let [ks   (set ks)
        anew #(CustomMap. %2 :filtered-map (mcall ^CustomMap %1 fns))]
    (CustomMap.
      m :filtered-map
      {:empty     (fn [^CustomMap this]
                    (empty (mcall this m)))
       :get       (fn [^CustomMap this k default]
                    (if (contains? ks k)
                      default
                      (let [result (get (mcall this m) k default)]
                        (if (and (map? result)
                                 (not (custom-map? result :filtered-map))
                                 deep)
                          (anew this result)
                          result))))
       :assoc     (fn [^CustomMap this k v]  (anew this (assoc  (mcall this m) k v)))
       :dissoc    (fn [^CustomMap this k]    (anew this (dissoc (mcall this m) k)))
       :keys      (fn [^CustomMap this]      (remove ks (keys (mcall this m))))
       :meta      (fn [^CustomMap this]      (meta (mcall this m)))
       :with-meta (fn [^CustomMap this meta] (anew this (with-meta (mcall this m)
                                                          meta)))})))


