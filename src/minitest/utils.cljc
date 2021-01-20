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
    `'~(ns-name *ns*)))
