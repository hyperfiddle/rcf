(ns dev-entrypoint
  (:require [example]
            [hyperfiddle.rcf :as rcf :refer [tests]]))

; enable tests after app namespaces are loaded (intended for subsequent REPL interactions)
(comment
  (rcf/enable!)
  )

; prevent test execution during cljs hot code reload
#?(:cljs (defn ^:dev/before-load stop [] (rcf/enable! false)))
#?(:cljs (defn ^:dev/after-load start [] (rcf/enable! true)))
