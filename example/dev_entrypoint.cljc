(ns dev-entrypoint
  (:require [example]
            [hyperfiddle.rcf :refer [tests]]))

#?(:cljs (defn ^:dev/before-load stop [] (set! hyperfiddle.rcf/*enabled* false)))
#?(:cljs (defn ^:dev/after-load start [] (set! hyperfiddle.rcf/*enabled* true)))

#?(:clj  (alter-var-root #'hyperfiddle.rcf/*enabled* (constantly true))
   :cljs (set! hyperfiddle.rcf/*enabled* true))
