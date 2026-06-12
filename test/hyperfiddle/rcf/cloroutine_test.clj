(ns hyperfiddle.rcf.cloroutine-test
  "rcf#56: guard for the cloroutine frontier. `%` inside a user fn/letfn body cannot be
   sequenced (cloroutine treats a break var in an escaping fn as an ordinary call, so it
   would never poll). RCF must refuse it loudly at compile time. The guard is pure
   AST-walking, so it is unit-testable on the JVM without a cljs compile."
  (:require [clojure.test :refer [deftest is testing]]
            [hyperfiddle.rcf]  ; interns `%`, so `hyperfiddle.rcf/%` resolves to a :var when analyzed in isolation
            [hyperfiddle.rcf.analyzer :as ana]
            [hyperfiddle.rcf.impl :as impl]))

(defn- analyze [form]
  ;; mirror impl/tests* — fn/letfn only become :fn/:letfn nodes after macroexpansion
  (binding [ana/*global-env* (ana/build-ns-map)]
    (->> (ana/analyze (ana/empty-env) form)
         (ana/resolve-syms-pass)
         (ana/macroexpand-pass))))

(deftest guard-refuses-%-inside-escaping-fns
  (testing "% directly inside a fn body is refused"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"rcf#56: % inside a fn"
          (#'impl/assert-no-%-in-fns! (analyze '(fn [] hyperfiddle.rcf/%))))))
  (testing "% inside a letfn-bound fn is refused (caught via the bound :fn)"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"rcf#56: % inside a fn"
          (#'impl/assert-no-%-in-fns! (analyze '(letfn [(g [] hyperfiddle.rcf/%)] (g)))))))
  (testing "% inside a reify method is refused"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"rcf#56: % inside a fn"
          (#'impl/assert-no-%-in-fns!
           (analyze '(reify clojure.lang.IDeref (deref [_] hyperfiddle.rcf/%)))))))
  (testing "% in a letfn BODY (not a bound fn) is allowed — cloroutine sequences it"
    (is (nil? (#'impl/assert-no-%-in-fns! (analyze '(letfn [(g [] 1)] (inc hyperfiddle.rcf/%)))))))
  (testing "% at top level (not inside a fn) passes the guard"
    (is (nil? (#'impl/assert-no-%-in-fns! (analyze '(inc hyperfiddle.rcf/%))))))
  (testing "a fn without % passes the guard"
    (is (nil? (#'impl/assert-no-%-in-fns! (analyze '(fn [x] (inc x))))))))
