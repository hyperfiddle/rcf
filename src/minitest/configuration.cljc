(ns minitest.configuration
  (:require
    #?(:clj [clojure.java.io         :as    io])
    #?(:clj [clojure.edn             :as    edn])
    #?(:clj [clojure.tools.reader    :as    r])
            [clojure.walk            :refer [prewalk postwalk]]
            [net.cgrand.macrovich    :as    macros]
            [minitest.base-config    :refer [base-config]]
            [minitest.utils          :refer [current-ns-name
                                             call
                                             ->|
                                             dissoc-in
                                             merge-entries-with
                                             minitest-var?
                                             current-bindings
                                             set-var!]]
            [minitest.custom-map     :refer [func-map filtered-map]])
  #?(:cljs
      (:require-macros
        [minitest.configuration      :refer [get-file-config defaccessors memo]]
        [minitest.with-bindings      :refer [with-bindings]]
        [minitest.utils              :refer [set-var!]])))

(declare config context with-config with-context)

(macros/deftime
  ;; TODO: clarify where the config file should be
  (defn config-file []
    (let [f (io/file "./minitest.edn")]
      (or (when  (and (.exists f) (-> f slurp edn/read-string empty? not))
            f)
          (io/resource "minitest.edn"))))

  (defmacro ^:private get-file-config []
    (some-> (config-file) slurp edn/read-string)))

(def #_once config-memo (atom {}))

(macros/deftime
  (defmacro memo [ks vals & body]
    `(let [full-path# (concat ~ks ~vals)
           already#   (get-in @config-memo full-path# ::not-found)]
       (if (= already# ::not-found)
         (doto (do ~@body)
               (->> (swap! config-memo assoc-in full-path#)))
         already#))))

(declare ^:dynamic *forced-context*
         ^:dynamic *forced-config*)

(def ^:dynamic reverse-merging-keys #{:FORCED-CTX :FORCED})
(def           reverse-merging-vars #{#'*forced-context*})

(defn deep-merge [& maps]
  (memo [:deep-merge] maps
        (reduce (partial
                  ;; TODO: move the memoization here and don't use reduce in config ?
                  merge-entries-with
                  (fn [[k x] [_ y]]
                    (let [reverse-k? (contains? reverse-merging-keys k)
                          mergeable? (every? #(or (map? %) (nil? %))
                                             [x y])]
                      (cond (and mergeable? reverse-k?) (deep-merge y x)
                            (and mergeable?)            (deep-merge x y)
                            reverse-k?                  x
                            :else                       y))))
                maps)))

(macros/deftime
  (defmacro ^:private defaccessors
    [name var-name & {:keys [getter setter clearer binder unbinder]
                      :or   {getter   true setter true clearer true binder true
                             unbinder true}}]
    (let [getter-name   name
          setter-name   (symbol (str             name "!"))
          clearer-name  (symbol (str "clear-"    name "!"))
          binder-name   (symbol (str "with-"     name))
          ;; Bad idea: only dissoc the var at hand and not the end config map.
          ; unbinder-name (symbol (str "without-"  name))
          ]
      `(do ~(when getter  `(defn ~getter-name [] ~var-name))
           ~(when setter  `(defn ~setter-name
                             ([k# v# & {:as more#}]
                              (~setter-name (assoc more# k# v#)))
                             ([m#]
                              (set-var! ~var-name (deep-merge ~var-name m#))
                              nil)))
           ~(when clearer `(defn ~clearer-name
                             ([]
                              (set-var! ~var-name nil)
                              nil)))
           ~(when binder  `(macros/deftime
                             (defmacro ~binder-name [~'m# & ~'body#]
                               (r/syntax-quote
                                 (binding
                                   [~var-name ~(if (-> var-name resolve
                                                       reverse-merging-vars)
                                                 `(deep-merge
                                                    (~'clojure.core/unquote ~'m#)
                                                    ~var-name)
                                                 `(deep-merge
                                                    ~var-name
                                                    (~'clojure.core/unquote ~'m#)))]
                                   (~'clojure.core/unquote-splicing
                                     ~'body#))))))
           ; ~(when unbinder `(macros/deftime
           ;                    (defmacro ~unbinder-name [~'ks# & ~'body#]
           ;                      (r/syntax-quote
           ;                        (binding
           ;                          [~var-name (dissoc-in
           ;                                       ~var-name
           ;                                       (~'clojure.core/unquote ~'ks#))]
           ;                          (~'clojure.core/unquote-splicing
           ;                            ~'body#))))))
           ))))

(def           file-config      (get-file-config))
(def ^:dynamic *early-config*   nil)
(def ^:dynamic *late-config*    nil)
(def ^:dynamic *early-context*  nil)
(def ^:dynamic *late-context*   nil)
(def ^:dynamic *forced-config*  nil)
(def ^:dynamic *forced-context* nil)
(def ^:dynamic *ns-configs*     nil)
(def ^:dynamic *ns-contexts*    nil)

(defaccessors default-config  *early-config* :getter false)
(defaccessors         config   *late-config* :getter false)
(defaccessors  forced-config *forced-config* :getter false)
(defaccessors     ns-configs *ns-configs*)

(defaccessors default-context  *early-context* :getter false)
(defaccessors         context   *late-context* :getter false)
(defaccessors  forced-context *forced-context* :getter false)
(defaccessors     ns-contexts    *ns-contexts*)

(defn default-config  [] (deep-merge (:DEFAULT (config :finalize false))
                                     *early-config*))
(defn forced-config   [] (deep-merge (:FORCED  (config :finalize false))
                                     *forced-config*))

(defn default-context [] (deep-merge (:DEFAULT-CTX (config :finalize false))
                                     *early-context*))
(defn forced-context  [] (deep-merge (:FORCED-CTX  (config :finalize false))
                                     *forced-context*))

(macros/deftime
  (defmacro ^:private rec [& body]
    `(recur (-> (context) :ns) ~@body)))

(defn ns-config
  ([]     (ns-config    (or (-> (context) :ns) (current-ns-name))))
  ([ns]   (deep-merge   (-> ns find-ns meta :minitest/config) (get (ns-configs) ns))))
(defn ns-context
  ([]     (ns-context   (or (-> (context) :ns) (current-ns-name))))
  ([ns]   (deep-merge   (-> ns find-ns meta :minitest/context) (get (ns-contexts) ns))))
(defn ns-config!
  ([m]    (ns-config!   (or (-> (context) :ns) (current-ns-name))  m))
  ([ns m] (ns-configs!  {ns m})))
(defn ns-context!
  ([m]    (ns-context!  (or (-> (context) :ns) (current-ns-name))  m))
  ([ns m] (ns-contexts! {ns m})))
(defn clear-ns-config!
  ([]     (clear-ns-config!  (or (-> (context) :ns) (current-ns-name))))
  ([ns]   (ns-configs!       (dissoc (ns-configs) ns))))
(defn clear-ns-context!
  ([]     (clear-ns-context! (-> (context) :ns)))
  ([ns]   (ns-contexts!      (dissoc (ns-contexts) ns))))
(macros/deftime
  (defmacro with-ns-config  [ns m & body] (with-ns-configs  {~ns ~m} ~@body))
  (defmacro with-ns-context [ns m & body] (with-ns-contexts {~ns ~m} ~@body)))






(macros/deftime
  ;; Ditto
  (defmacro extending-config [cfg & body]
    (let [ctx-k (-> (gensym "at-runtime-") keyword)]
      `(with-config
         {:CTX  {~ctx-k true}
          :WHEN {~ctx-k {true (let [~'&config (with-context {~ctx-k false}
                                                (config))] ;; TODO: use lay
                                ~cfg)}}}
         ~@body))))

(defn- parse-WHEN-val [m x]
  (condp call x
    map?        x
    sequential? (->> (map (partial parse-WHEN-val m) x)
                     (reduce deep-merge))
    (do         (some-> m :WHEN (get x)
                        (->> (parse-WHEN-val m))))))

(defn contextualize [m ctx]
  (memo
    [:contextualize] [m ctx]
    (->> m
         (postwalk
           (fn [form]
             (if-not (and (map? form)
                          (or (contains? form :WHEN)
                              (contains? form :FORCED)))
               form
               (let [when-map   (:WHEN form)
                     active-ctx (deep-merge (:CTX form)
                                            (select-keys ctx (keys when-map))
                                            #_(:FORCED-CTX form))
                     ms         (map #(parse-WHEN-val form (get-in when-map %))
                                     active-ctx)
                     forced-map (:FORCED form)
                     new-form   (reduce deep-merge
                                        (concat [form] ms [forced-map]))]
                 ;; Since a when-map can bring in another one, contextualize
                 ;, again until fix point is reached
                 (if (= form new-form)
                   new-form
                   (contextualize new-form ctx)))))))))

(defn context [& [default-ctx]]
  (let [cfg (delay (config :finalize false))] ;; TODO: use lay
    (deep-merge *early-context*
                (or default-ctx (:CTX @cfg))
                *late-context*
                (:FORCED-CTX (or default-ctx @cfg))
                *forced-context*)))

(defn  config [& {:keys [finalize] :or {finalize true}}]
  (let [srcs     [base-config
                  file-config
                  *early-config*
                  (ns-config (or (-> (context {}) :ns) (current-ns-name)))
                  *late-config*
                  *forced-config*]
        ;; deep-merging 2 at a time to better benefit from memoization
        merged   (reduce deep-merge srcs)
        base-ctx (reduce (fn [acc k]
                           (-> (get merged k)
                               (as-> $ (deep-merge acc (contextualize $ $)))))
                         {} [:CTX :FORCED-CTX :FORCED])
        ctx      (context (deep-merge merged base-ctx))
        result   (->> (concat srcs [base-ctx *forced-config*])
                      (map #(contextualize % ctx))
                      (reduce deep-merge))]
    (if finalize
      (-> result
          (filtered-map [:CTX :WHEN :DEFAULT :DEFAULT-CTX :FORCED-CTX :FORCED]) ;; TODO: really ?
          (func-map)) ;; TODO move func-map out of finalize ?
      result)))


(def ^:dynamic *surrounding-config-bindings* nil)

(def config-dyn-vars
  [#'*early-config*
   #'*late-config*
   ; #'*forced-config* ;; These two purposefully excluded
   #'*early-context*
   #'*late-context*
   ; #'*forced-context*
   ])

(def blank-config-bindings
  (into {} (for [v config-dyn-vars]
             [v nil])))

(defn blank-config
  ([]   (blank-config (-> (context) :ns)))
  ([ns] (with-bindings blank-config-bindings
          (->> (with-context {:ns ns} (config))
               (postwalk (fn [x] ;; TODO: keep ?
                           (if (map? x)
                             (into {} x)
                             x)))))))

(defn current-config-bindings []
  (->> config-dyn-vars
       (map (->| (juxt identity deref) vec))
       (into {})))

(defn current-case-bindings []
  (let [config-var? (set (keys (current-config-bindings)))] ;; TODO: useful ?
    (->> (current-bindings)
         (remove (->| key (some-fn config-var? minitest-var?
                                   #{#'*1 #'*2 #'*3 #'*e})))
         (into {}))))
