(ns minitest.with-bindings
  "Cljs implementation of clojure.core/with-bindings* as well as:
  - with-bindings
  - bound-fn* & bound-fn
  - var-set"
  #?(:clj (:refer-clojure :exclude [with-bindings bound-fn]))
  (:require  [net.cgrand.macrovich       :as macros]
    #?(:cljs [applied-science.js-interop :as j])
    #?(:cljs [clojure.string             :as str])))

#?(:cljs
    (do (defn var-name [var]
          (symbol (-> var meta :ns) (-> var meta :name)))

        (defn js-name [var]
          (munge (str (-> var meta :ns) "." (-> var meta :name))))

        (def ^:private bindings-jsa
          "Joint security area to convoy data between Cljs and runtime evaled JS."
          (js-obj))

        (def ^:private ^:export bindings-jsa-name
          (-> `bindings-jsa str (str/replace "/" ".") munge))

        (defn var-set [var val]
          (let [id      (str (gensym))
                js-name (js-name var)]
            ;; Since marshalling val into JS data and using it in the evaluated
            ;; string is too limited/difficult, we store it in the JSA using Cljs
            ;; and read from it in the JS evaluated string.
            (j/assoc! bindings-jsa id val)
            (js/eval (str js-name " = " bindings-jsa-name \[ \" id \" \]))
            (j/assoc! bindings-jsa id nil)))

        (defn with-bindings* [bindings-map f & args]
          (let [orig  (->> (keys bindings-map)
                           (map (juxt identity deref))
                           (into {}))]
            (doseq [[var val] bindings-map]
              (var-set var val))
            (try (apply f args)
              (finally
                (doseq [[var orig-val] orig]
                  (var-set var orig-val))))))

        (defn get-all-dyn-bindings []
          nil
          #_(->> (all-ns)
                 (mapcat ns-interns)
                 (map val)
                 (filter (comp :dynamic meta))
                 (map (comp vec (juxt identity deref)))
                 (into {})))

        (defn bound-fn*
          "Clojure.core implementation uses `get-thread-bindings` which relies on
          concepts not event present in Cljs. Instead of only preserving the value
          of vars that were bound we have to resort to doing the same with
          all the dynamic vars, whether they are still bound to their root value
          or not.
          So this implementation is noticeably slower than its Clj counterpart,
          although only by a constant factor. Beware if you use this for pervasive,
          low-level purposes."
          [f]
          (let [bindings (get-all-dyn-bindings)]
            (fn [& args]
              (apply with-bindings* bindings f args))))))

(macros/deftime
  (defmacro with-bindings [bindings-map & body]
    `(minitest.with-bindings/with-bindings* ~bindings-map (fn [] ~@body)))

  (defmacro bound-fn
    "See [[bound-fn*]]."
    [& fn-tail]
    `(minitest.with-bindings/bound-fn* (fn ~@fn-tail))))
