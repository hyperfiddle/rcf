(ns minitest.dbg
  (:require [net.cgrand.macrovich :as macros]
    #?(:clj [clojure.java.io      :as io])))

(macros/deftime
  (def ^:dynamic *debug* false)

  (macros/case
    :clj (defmacro dbg [& args]
           (when *debug*
             `(binding [*out* (io/writer java.lang.System/out)]
                (println ~@args))))))
