(require '[cljs-test-runner.main :as r]
         '[clojure.java.io       :as io])

(import 'java.io.File)

(def- out-dir
  (doto (io/file (File/createTempDir "minitest") "clojurescript")
        io/make-parents
        .mkdir))

(defn- xxxx []
  (let [runner   (r/render-test-runner-cljs {})
        gen-dir  (io/file out-dir "gen")
        gen-path (.getAbsolutePath gen-dir)
        src-path (.getAbsolutePath
                   (io/file gen-dir "cljs_test_runner" "gen.cljs"))
        out-path (.getAbsolutePath (io/file out "cljs_test_runner.gen.js"))]
    ))
