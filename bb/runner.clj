(ns runner
  (:require
   [clojure.test :as test]
   [hyperfiddle.rcf :as rcf]))

(defn run-tests [_]
  (alter-var-root (var rcf/*generate-tests*) (constantly true))
  (let [test-nses '[hyperfiddle.rcf-test
                    hyperfiddle.rcf.analyzer-test
                    ;; needs missionary, doesn't work due to missing reactive streams Java lib:
                    #_hyperfiddle.rcf.unify-test
                    ;; needs missionary:
                    #_hyperfiddle.rcf.example-test
                    ]]
    (apply require test-nses)
    (let [{:keys [fail error]}
          (apply test/run-tests test-nses)]
      (when (and fail error (pos? (+ fail error)))
        (throw (ex-info "Tests failed" {:babasha/exit 1}))))))
