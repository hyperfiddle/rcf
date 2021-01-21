(ns minitest.utils
  (:require [net.cgrand.macrovich :as macros]))

(def ->|               #(apply comp (reverse %&)))
(def call              #(apply %1 %&))
(def as-form           #(do `'~%))
(def as-thunk          #(do `(fn [] ~%)))
(def as-wildcard-thunk #(do `(fn [] (let [~'_ '~'_] ~%))))

(macros/deftime
  (defmacro current-file []
    (if (-> *file* first (= \/))
      *file*
      (-> *file* ClassLoader/getSystemResource .getPath)))

  (defmacro current-ns-name []
    `'~(ns-name *ns*))

  ;; Taken from:
  ;; https://github.com/ptaoussanis/encore/blob/009da50b8d38ebd00db0ec99bd99312cf0c56203/src/taoensso/encore.cljc#L374
  (defmacro defalias
    "Defines an alias for a var, preserving its metadata."
    ([    src      ] `(defalias ~(symbol (name src)) ~src nil))
    ([sym src      ] `(defalias ~sym                 ~src nil))
    ([sym src attrs]
     (let [attrs (if (string? attrs) {:doc attrs} attrs)] ; Back compatibility
       `(let [attrs# (conj (select-keys (meta (var ~src))
                                        [:doc :arglists :private :macro])
                           ~attrs)]
          (alter-meta! (def ~sym @(var ~src)) conj attrs#)
          (var ~sym))))))
