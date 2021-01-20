(ns minitest.clojurescript
  (:require [clojure.tools.reader :as r]
            [net.cgrand.macrovich :as macros]
    #?(:clj [clojure.java.io      :as io])
    #?(:clj [net.cgrand.macrovich :as macros])))

;; TODO: handle multiple projects running minitest at the same time
(macros/deftime
    (def cljs-tmp-dir
      (doto (io/file (System/getProperty "java.io.tmpdir") "minitest" "cljs")
            .mkdir))

    (defmacro cljs-src-path []
      (.getAbsolutePath (io/file cljs-tmp-dir "src")))

    (defmacro cljs-out-path []
      (.getAbsolutePath (io/file cljs-tmp-dir "out"))))

(defn- read-forms [r]
  (->> (iterate (fn [[r form]]  [r (->> r (r/read {:read-cond :allow
                                                   :features #{(macros/case
                                                                 :clj  :clj
                                                                 :cljs :cljs)}
                                                   :eof ::eof}))])
                [r nil])
       rest
       (map second)
       (take-while #(not= % ::eof)))
  ; (->> (iterate (fn [[r forms]]
  ;                 (let [start-line (.getLineNumber    r)
  ;                       start-col  (.getColumnNumber  r)
  ;                       form       (read {:eof ::eof} r)]
  ;                   (when-not (= form ::eof)
  ;                     [r (conj forms {:line start-line
  ;                                     :col  start-col
  ;                                     :form form})]))))
  ;      (keep identity))
  )

(defn read-forms-upto [f {:keys [line column]}]
  #?(:clj (with-open [r (clojure.lang.LineNumberingPushbackReader.
                          (io/reader f))]
            (->> (read-forms r)
                 (take-while (every-pred #(-> % meta :line   (<= line))
                                         #(-> % meta :column (<= column))))
                 doall))))




; (defn- xxxx []
;   (let [runner   (r/render-test-runner-cljs {})
;         gen-dir  (io/file out-dir "gen")
;         gen-path (.getAbsolutePath gen-dir)
;         src-path (.getAbsolutePath
;                    (io/file gen-dir "cljs_test_runner" "gen.cljs"))
;         out-path (.getAbsolutePath (io/file out "cljs_test_runner.gen.js"))]
;     ))
