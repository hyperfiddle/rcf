(ns minitest.orchestrator
  (:require [net.cgrand.macrovich             :as           macros]
            [minitest.higher-order #?@(:clj  [:refer        [outside-in->>
                                                             with-context|]]
                                       :cljs [:refer-macros [outside-in->>
                                                             with-context|]])]
            [minitest.config       #?@(:clj  [:refer        [config
                                                             with-config]
                                              :as           config]
                                       :cljs [:refer        [config]
                                              :refer-macros [with-config]
                                              :as           config])]
            [minitest.utils                   :refer        [->|]]
   #?(:cljs [minitest.with-bindings           :refer        [with-bindings*]]))
  #?(:cljs
      (:require-macros
        [minitest.orchestrator        :refer        [let-testing-fns
                                                     ensuring-testing-state]])))

(def ^:dynamic ^:no-doc *testing-state* nil)

(defn- fail-fast! [state ns rpt]
  ;; since the ex we are about to throw will be printed on *err*, and to avoid
  ;; a printing race condition with *out* when it's bound to it, we need to
  ;; force the past tests to actually print to the screen before throwing the ex
  ;; for this one.
  (flush)
  (throw (ex-info
           (str "Test " (name (:status rpt)) ":\n"
                (with-config {:out *out*}
                  (let [report-fn (-> (config) :report-fn)]
                    (with-out-str
                      (report-fn state :after :case ns rpt)))))
           (assoc rpt :type :minitest/fail-fast))))

;; TODO: remove
(macros/deftime
  (defmacro ^:private ensuring-testing-state [& body]
    `(binding [*testing-state* (or *testing-state* (atom {}))]
       ~@body))

  (defmacro ^:private let-testing-fns [conf & body]
    `(let [{~'&run :run-fn ~'&execute :execute-fn ~'&report :report-fn} ~conf]
       ~@body))

  ;; TODO: keep ?
  (defmacro ^:private doseq-each-lang [conf & body]
    `(let [conf# ~conf]
       (doseq [l# (:langs conf#)]
         (let-testing-fns conf#
           ~@body))))

  (defmacro ^:private for-each-lang [conf & body]
    `(->> (let [conf# ~conf]
            (for [l# (:langs conf#)]
              (let-testing-fns conf#
                [l# (do ~@body)])))
          (into {}))))

(defn handling-case-bindings| [f]
  (fn [s l n d]
    (if (= l :case)
      (with-bindings* (-> d :bindings)
        f s l n d)
      (f s l n d))))

(defn handling-case-config-bindings| [f]
  (fn [s l n d]
    (if (= l :case)
      (with-bindings* (->> (for [[var val] (-> d :config-bindings)]
                             [var (config/deep-merge @var val)])
                           (into {}))
        f s l n d)
      (f s l n d))))

(defn handling-case-config| [fetch f]
  (fn [s l n d]
    (if (= l :case)
      (with-config (fetch d)
        (f s l n d))
      (f s l n d))))

(defn orchestrate-level [state level ns data
                         & {:keys [handle-before handle-after]}]
  (let-testing-fns (config)
    (let [fetch-case-config-before #(merge (-> %         :form           meta)
                                           (-> %         :macroexpanded  meta)
                                           (-> % :tested :form           meta)
                                           (-> % :tested :macroexpanded  meta))
          fetch-case-config-after  #(-> (fetch-case-config-before %)
                                        (merge (-> % meta :minitest/config)))
          handle-before (with-context|
                          {:position :before}
                          (handling-case-bindings|
                            (handling-case-config-bindings|
                              (handling-case-config|
                                fetch-case-config-before
                                (or handle-before
                                    (fn [s l n d]
                                      (&report  s :before l n d)
                                      (&execute s :before l n d)))))))
          handle-after  (with-context|
                          {:position :after
                           :location   (if (= &level :case)
                                         (:location &data) :minitest/not-set!)
                           :status     (if (= &level :case)
                                         (:status   &data) :minitest/not-set!)}
                          (handling-case-bindings|
                            (handling-case-config-bindings|
                              (handling-case-config|
                                fetch-case-config-after
                                (or handle-after
                                    (fn [s l n d]
                                      (&execute s :after l n d)
                                      (&report  s :after l n d)))))))]
      (handle-before       state level ns data)
      (let [run-f    (handling-case-bindings|
                       (handling-case-config-bindings|
                         (handling-case-config|
                           fetch-case-config-before
                           &run)))
            rpt-data (run-f state level ns data)]
        (handle-after      state level ns rpt-data)))))

(defn install-config-bindings| [f]
  (fn [s l n d]
    (let [bindings-map (-> (config) :bindings)]
      (if (seq bindings-map)
        (with-bindings* bindings-map f s l n d)
        (f s l n d)))))

(defn handling-fail-fast| [f]
  (fn [s l n d]
    (if-not (= l :case)
      (f s l n d)
      (orchestrate-level
        s l n d
        :handle-after
        (fn [s l n d]
          (let [conf (config)]
            (let-testing-fns conf
              (if (and (:fail-fast conf)
                       (some-> d :status #{:error :failure}))
                (fail-fast! s n d)
                (do (&execute s :after l n d)
                    (&report  s :after l n d))))))))))

(def orchestrate
  (outside-in->>
    (with-context|
      {:test-level &level
       :ns         &ns
       :lang       (macros/case  :clj :clj  :cljs :cljs)})
    install-config-bindings|
    handling-fail-fast|
    orchestrate-level))

(defn run-execute-report!
  ([level ns->tsts]
   (run-execute-report! level nil ns->tsts))
  ([level ns data]
   (let [orchestrate-fn (-> (config) :orchestrate-fn)]
     (ensuring-testing-state (orchestrate-fn *testing-state* level ns data)))))
