(ns minitest.higher-order
  (:require [net.cgrand.macrovich       :as           macros]
            [minitest.utils             :refer        [->|]]
            [minitest.config            :refer        [context
                                                       config]
                                        :as           config])
  #?(:cljs (:require-macros
             [minitest.higher-order     :refer        [def-on-fn
                                                       def-config-binders
                                                       anafn
                                                       outside-in->>]]
             [minitest.with-bindings    :refer        [with-bindings*]])))

(def report-actions   [:output    :report   :explain])
(def report-actioneds [:outputted :reported :explained])

(macros/deftime
  (defmacro def-on-fn [name first-arg pred-expr]
    (let [f-sym        (gensym "f")
          continue-sym (gensym "continue")]
      `(defn ~name [~first-arg ~f-sym & [~continue-sym]]
         (fn
           ~@(for [args '[;; This arity for orchestrate-fn
                          [&state &level &ns &data]
                          ;; This arity for run-fn, execute-fn & report-fn
                          [&state &position &level &ns &data]]]
               `(~args
                  (let [~'&position ~(if (.contains args '&position)
                                       '&position
                                       nil)]
                    (if ~pred-expr
                      (let [new-data# (~f-sym ~@args)]
                        (if ~continue-sym
                          (~continue-sym ~@(butlast args) new-data#)
                          new-data#))
                      (if ~continue-sym
                        (~continue-sym ~@args)
                        ~'&data)))))))))

  (defmacro anaph| [f]
    `(fn
       ([      ~'&state             ~'&level ~'&ns ~'&data]
        (let           [~'&position nil
                        ~'&args     [~'&state             ~'&level ~'&ns ~'&data]]
          (~f  ~'&state             ~'&level ~'&ns ~'&data)))
       ([      ~'&state ~'&position ~'&level ~'&ns ~'&data]
        (let           [~'&args     [~'&state ~'&position ~'&level ~'&ns ~'&data]]
          (~f    ~'&state ~'&position ~'&level ~'&ns ~'&data)))))

  (defmacro anafn [& body]
    `(fn
       ([      ~'&state             ~'&level ~'&ns ~'&data]
        (let     [~'&position nil
                  ~'&args     [~'&state             ~'&level ~'&ns ~'&data]]
          ~@body))
       ([      ~'&state ~'&position ~'&level ~'&ns ~'&data]
        (let   [~'&args       [~'&state ~'&position ~'&level ~'&ns ~'&data]]
          ~@body))))

  (defmacro def-config-binders []
    `(do ~@(for [w-wo     ['with 'without]
                 name     ['config 'context]
                 modifier [nil 'default 'forced]
                 :let [full-name (str w-wo \-
                                      modifier (when modifier \-)
                                      name)]]
             `(defmacro ~(symbol (str full-name "|"))
                [arg# f#]
                `(anaph| (fn [& ~'args#]
                           (~'~(symbol "minitest.config" full-name)
                                ~arg# (apply ~f# ~'args#))))))))

  (def-config-binders)

  (defn with-bindings| [m f]
    (fn [& args]
      (apply with-bindings* m f args)))

  (defmacro outside-in->> [& frms] `(->> ~@(reverse frms)))
  (defmacro when|            [e f] `(anaph| #(if ~e (apply ~f %&) ~'&data)))
  (defmacro if|        [e f & [g]] `(anaph| #(if ~e
                                               (apply ~f %&)
                                               (apply (or ~g do-nothing) %&)))))

(defn apply| [f]
  #(apply f %))

(defn chain|
  ([f g]
   (fn [& args]
     (let [result (apply f args)]
       (apply g (concat (butlast args) [result])))))
  ([f g & more]
   (chain| f (apply chain| g more))))

(def level-below
  {:suite :ns
   :ns    :block
   :block :case})

(def level-above
  {:ns    :suite
   :block :ns
   :case  :block})

(defn levels-below [l]
  (->> (iterate level-below l)
       (take-while identity)
       rest))

(defn levels-above [l]
  (->> (iterate level-above l)
       (take-while identity)
       rest))

(defn do-nothing
  ([s l n d]    d)
  ([s p l n d]  d))

;; For cljs
(declare on-level|)
(declare on-context|)
(declare on-config|)

(defn match-level?| [position-level]
  (anafn
    (cond (set? position-level) (or (position-level [&position &level])
                                    (position-level &level)
                                    (position-level [&level])
                                    (position-level &position)
                                    (position-level [&position]))
          (fn? position-level)  (position-level &state &position
                                                &level &ns &data)
          :else                 (or (= position-level [&position &level])
                                    (= position-level &level)
                                    (= position-level [&level])
                                    (= position-level &position)
                                    (= position-level [&position])))))

(def-on-fn on-level|   position-level ((match-level?| position-level)
                                       &state &position &level &ns &data))
(def-on-fn on-context| expected-ctx   (= expected-ctx
                                         (select-keys (context)
                                                      (keys expected-ctx))))
(def-on-fn on-config|  expected-cfg   (= expected-cfg
                                         (select-keys (config)
                                                      (keys expected-cfg))))

(defn continue-when| [pred continue]
  (fn [& args]
    (if (apply pred       args)
      (apply   continue   args)
      (apply   do-nothing args))))

(defn stop-when| [pred continue]
  (continue-when| (complement pred) continue))

(defn instead-of-level| [level action continue]
  (outside-in->> (on-level| level action)
                 (stop-when| (match-level?| level))
                 continue))

(defn marking-as| [k f]
  (anafn
    (if (= &level :case)
      (-> (apply f &args)
          (assoc k true))
      (apply f &args))))
