(ns hyperfiddle.rcf-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            #_[hyperfiddle.rcf.analyzer :as ana])
  #?(:clj (:import [clojure.lang ExceptionInfo]))
  #?(:cljs (:require-macros [hyperfiddle.rcf-test])))

(deftest tap-outside-tests
  (is (= (with-out-str (rcf/tap 1)) "1\n"))
  (is (= (rcf/tap 1) 1)))

(defmacro my-def [x]
  `(def ~(vary-meta x assoc 
                    :form3 (inc 1)         ; evaluated now
                    :form4 (quote (inc 1)) ; interpreted as if evaluated after code emission  
                    :form5 (quote (quote (inc 1))) ; escaping interpretation
                    )))

#?(:clj
   (tests
     "Custom var meta on def symbol are interpreted as if they were evaluated after emission."
     ;; CLJ only, no vars in cljs.
     (my-def ^{:form1 (quote (inc 1))
               :form2 (inc 1)}        ; read and evaluated as usual
       x)
     (:form1 (meta #'x)) := '(inc 1)
     (:form2 (meta #'x)) := 2
     (:form3 (meta #'x)) := 2
     (:form4 (meta #'x)) := 2
     (:form5 (meta #'x)) := '(inc 1)))


(tests
  "Support for variadic fn" ; issue #65
  (let [f (fn ([a]        [a])          ; also check multi arity
            ([a b]      [a b])
            ([a b & cs] [a b cs]))
        g (fn [& args] args)
        h #(vector %&)]
    (f 1)       := [1]
    (f 1 2)     := [1 2]
    (f 1 2 3 4) := [1 2 '(3 4)]
    (g 1 2)     := [1 2]
    (h 1 2)     := ['(1 2)]
    ))

(tests
  "Inline letfn support"
  (letfn [(descent  [x] (cond (pos? x) (dec x)
                              (neg? x) (inc x)
                              :else    x))
          (is-even? [x] (if (zero? x) true  (is-odd?  (descent x))))
          (is-odd?  [x] (if (zero? x) false (is-even? (descent x))))]
    [(is-even? 0) (is-even? 1) (is-even? 2) (is-even? -2)] := [true false true true]
    [(is-odd?  0) (is-odd?  2) (is-odd?  3) (is-odd? -3)]  := [false false true true]))

(tests
  "! still works"
  (rcf/! 5)
  rcf/% := 5)

(tests
  ":throws works in clj(s)"
  ;; inlining `thrower` leads to "unreachable code" warning
  (let [thrower #(throw (ex-info "boom" {}))]
    (thrower) :throws ExceptionInfo))

;; For an unknown reason, `macroexpand-1` acts as identity when runnning
;; tests without a repl.

;; (defn disable-ci [f]
;;   (let [gen rcf/*generate-tests*
;;         enabled rcf/*enabled*]
;;     (alter-var-root #'rcf/*generate-tests* (constantly (not gen)))
;;     (alter-var-root #'rcf/*enabled* (constantly (not enabled)))
;;     (f)
;;     (alter-var-root #'rcf/*generate-tests* (constantly gen))
;;     (alter-var-root #'rcf/*enabled* (constantly enabled))))
;; 
;; (t/use-fixtures :once disable-ci)
;; 
;; (defn tests' [& [body]]
;;   (apply #'tests nil nil body))
;; 
;; (deftest block-behavior
;;   (testing "`tests` behaves like"
;;     (testing "`cc/comment` when RCF is disabled."
;;       (binding [rcf/*enabled* false]
;;         (is (nil? (tests' '(1 := 1))))))
;;     (testing "`cc/do` when RCF is enabled."
;;       (is (= '(do) (tests')))
;;       (is (= '1 (tests' '(1))))
;;       (is (= '(do 1 2) (tests' '(1 2)))))))
;; 
;; (deftest nesting
;;   (testing "Nested `tests` flattens"
;;     (is (= '(do) (tests' '((tests)))))
;;     (is (= '(do 1 2) (tests' '(1 (tests 2)))))))
;; 
;; #_(deftest documentation-behavior
;;   (testing "`tests` behaves like `t/testing` when a string litteral is followed by an expression."
;;     (is (= `(t/testing "a" 1)
;;            (macroexpand-1 '(tests "a" 1))))))
;; 
;; #_(deftest basic-assertion
;;   (testing "Assertion sigils are listed by `t/assert-expr`."
;;     (is (contains? (methods t/assert-expr) :default)))
;;   (testing "Infix sigils desugares to `is`"
;;     (is (= `(t/is (:default 1 1))
;;            (macroexpand-1 '(tests 1 :default 1)))))
;;   (testing "RCF rewrites some sigils to avoid conflicts" ;; extensible by a multimethod
;;     (is (= '(clojure.test/is (:hyperfiddle.rcf/= '_ '_))
;;            (macroexpand-1 '(tests _ := _))))))
;; 
;; (comment
;;   (alter-var-root #'rcf/*generate-tests* (constantly false))
;;   (rcf/enable! true))

;; (deftest repl-behavior
;;   (testing "`tests` behaves like a REPL"
;;     (testing "where *1, *2, *3 referers to previous results."
;;       (is (= '(i/repl 1 *1 *2 *3) (tests* 1 *1 *2 *3))))
;;     (testing "where a `def` form is immediately available after being evaluated."
;;       (is (= '(i/repl (def a 1) a) (tests* (def a 1) a)))
;;       (is (= '(i/repl ((def a identity) 1))
;;              (tests* ((def a identity) 1)))))))
;; 
;; (deftest async-behavior
;;   (testing "`tests` can probe for values using `!`"
;;     (is (= '(let [[! %] (i/queue)]
;;               (! 1))
;;            (tests* (! 1)))))
;;   (testing "`tests` can inspect probed values using `%`"
;;     (is (= '(let [[! %] (i/queue)]
;;               (! 1)
;;               (%))
;;            (tests* (! 1) %)))
;;     (is (= '(let [[! %] (i/queue)]
;;               (! 1)
;;               (% (fn [result] (i/is (:= result 1)))))
;;            (tests* (! 1) % := 1)))))
;; 
;; (deftest repl-macro
;;   (testing "RCF repl behavior"
;;     (testing "supports *1 *2 *3"
;;       (is (= '(let [[push! peek] (i/binding-queue)]
;;                 (push! 1)
;;                 (push! (peek 0))
;;                 (push! (peek 1))
;;                 (peek 2))
;;              '(i/repl 1 *1 *2 *3))))
;;     (testing "handles the Gilardi scenario"
;;       (is (= '(do (def a 1) a)
;;              (tests* (def a 1) a)))
;;       (is (= '(do (def a identity) (a 1))
;;              (tests* ((def a identity) 1)))))))
;; 
;; (macroexpand-1 `(tests* 1 := 1))

;; (macroexpand-1 `(tests* (do (rcf/! 1) rcf/% := 1)))

