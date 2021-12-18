(ns hyperfiddle.rcf.cljs-analyzer
  (:refer-clojure :exclude [resolve]))

(defn- load-deps! []
  #?(:clj (require '[cljs.analyzer] '[cljs.analyzer.api])))

(defn resolve [& args]
  (load-deps!)
  #?(:clj (apply (clojure.core/resolve 'cljs.analyzer.api/resolve) args)))

(defn load-tests? []
  (load-deps!)
  #?(:clj @(clojure.core/resolve 'cljs.analyzer/*load-tests*)))

