(require '[clojure.java.io      :as io]
         '[clojure.tools.reader :as r])

(import 'java.nio.file.Files)
(import 'clojure.lang.LineNumberingPushbackReader)

(def cljs-gen-tmp-dir
  (doto (io/file (System/getProperty "java.io.tmpdir") "minitest" "cljs")
        .mkdir))

(def cljs-gen-src-path (.getAbsolutePath (io/file cljs-gen-tmp-dir "src")))
(def cljs-gen-out-path (.getAbsolutePath (io/file cljs-gen-tmp-dir "out")))

(defn- read-forms [r]
  (->> (iterate (fn [[r form]]  [r (r/read {:eof ::eof :read-cond :allow} r)])
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
  (with-open [r (clojure.lang.LineNumberingPushbackReader.
                  (io/reader f))]
    (->> (read-forms r)
         (take-while (every-pred #(-> % meta :line   (<= line))
                                 #(-> % meta :column (<= column))))
         doall)))







; (defn- xxxx []
;   (let [runner   (r/render-test-runner-cljs {})
;         gen-dir  (io/file out-dir "gen")
;         gen-path (.getAbsolutePath gen-dir)
;         src-path (.getAbsolutePath
;                    (io/file gen-dir "cljs_test_runner" "gen.cljs"))
;         out-path (.getAbsolutePath (io/file out "cljs_test_runner.gen.js"))]
;     ))
