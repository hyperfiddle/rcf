(ns dev-entrypoint
  (:require [example]
            [hyperfiddle.rcf :refer [tests]]))

; enable tests after app namespaces are loaded (intended for subsequent REPL interactions)
#?(:clj  (alter-var-root #'hyperfiddle.rcf/*enabled* (constantly true))
   :cljs (set! hyperfiddle.rcf/*enabled* true))

; prevent test execution during cljs hot code reload
#?(:cljs (defn ^:dev/before-load stop [] (set! hyperfiddle.rcf/*enabled* false)))
#?(:cljs (defn ^:dev/after-load start [] (set! hyperfiddle.rcf/*enabled* true)))
