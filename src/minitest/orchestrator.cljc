(ns minitest.orchestrator
  (:require [net.cgrand.macrovich             :as           macros]
            [minitest.higher-order #?@(:clj  [:refer        [chain|
                                                             apply|
                                                             with-bindings|
                                                             outside-in->>
                                                             with-context|
                                                             minifn
                                                             if|]]
                                       :cljs [:refer        [chain|
                                                             apply|
                                                             with-bindings|]
                                              :refer-macros [outside-in->>
                                                             with-context|
                                                             minifn
                                                             if|]])]
            [minitest.configuration           :refer        [config
                                                             context]
                                              :as           config]
            [minitest.utils        #?@(:clj  [:refer        [->|
                                                             with-out-str+result]]
                                       :cljs [:refer        [->|]
                                              :refer-macros [with-out-str+result]])]
            [minitest.walk                    :refer        [copostwalk]]
   #?(:cljs [minitest.with-bindings           :refer        [with-bindings*]
                                              :refer-macros [with-bindings]]))
  #?(:cljs
      (:require-macros
        [minitest.orchestrator        :refer        [let-testing-fns
                                                     ensuring-testing-state]])))

(def ^:dynamic ^:no-doc *testing-state* nil)

(defn- fail-fast! [state level ns rpt]
  ;; since the ex we are about to throw will be printed on *err*, and to avoid
  ;; a printing race condition with *out* when it's bound to it, we need to
  ;; force the past tests to actually print to the screen before throwing the ex
  ;; for this one.
  (flush)
  (throw (ex-info
           (str "Test " (name (:status rpt)) ":\n"
                (config/with-config {:print-to ;; TODO: keep ?
                                     #?(:clj  *out*
                                        :cljs cljs.core/*print-fn*)}
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

(defn handling-captured-bindings| [f]
  (outside-in->>
    (if| (= &level :case)
      (minifn (apply with-bindings* (-> &data       :bindings) f &args)))
    ;; TODO
    ; (if| (and (= &level :block)
    ;           (-> (context) :exec-mode (not= :inner-test)))
    ;   (minifn (apply with-bindings* (-> &data first :bindings) f &args)))
    f))

(defn handling-case-execution-output| [f]
  (if| (= &level :case)
    (minifn (let [[output result] (with-out-str+result (apply f &args))]
             (update result :output str output)))
    f))

(defn get-captured-config-bindings [data]
  (->> (for [[var val] (-> data :config-bindings)
             :let [varv @var]]
         [var
          (config/deep-merge varv val)
          ;; TODO: for cljs
          #_(config/deep-merge
            @var
            (first (copostwalk (fn [x surrounding]
                                 [(if (map-entry? x)
                                    (if (not= (val x) (val surrounding))
                                      x
                                      nil)
                                    x)
                                  surrounding])
                               val
                               (get config/*surrounding-config-bindings* var))))])
       (into {})))

(defn handling-bindings-from-config [f]
  (minifn
    (let [bindings-map (-> (config) :bindings)]
      (if (seq bindings-map)
        (apply with-bindings* bindings-map f &args)
        (apply f &args)))))

(defn binding-test-output| [f]
  (minifn
    (binding [#?(:clj *out* :cljs cljs.core/*print-fn*)
              (-> (config) :print-to)]
      (apply f &args))))

(defn handling-captured-config| [f]
  (outside-in->>
    (if| (= &level :case)
      (with-bindings| (get-captured-config-bindings &data)
        f))
    f))

;; TODO: remove
(defn handling-config-from-meta| [fetch f]
  (fn [s l n d]
    (if (= l :case)
      (config/with-config (fetch d)
        (f s l n d))
      (f s l n d))))

(defn handling-case-specific-context| [pos f]
  (with-context| {:position pos
                  :type     (when (= &level :case) (:type   &data))
                  :status   (when (= &level :case) (:status &data))}
    f))

(defn orchestrate-level [state level ns data
                         & {:keys [handle-before handle-after]}]
  (let-testing-fns (config)
    (let [fetch-case-config-before #(merge (-> %         :form           meta)
                                           (-> %         :macroexpanded  meta)
                                           (-> % :tested :form           meta)
                                           (-> % :tested :macroexpanded  meta))
          fetch-case-config-after  #(-> (fetch-case-config-before %)
                                        (merge (-> % meta :minitest/config)))
          handle-before (handling-case-specific-context| :before
                          (handling-config-from-meta|
                            fetch-case-config-before
                            (or handle-before
                                (->| (minifn [&state :before &level &ns &data])
                                     (apply|
                                       (chain| &report
                                               (handling-case-execution-output|
                                                 &execute)))))))
          handle-after  (handling-case-specific-context| :after
                          (handling-config-from-meta|
                            fetch-case-config-after
                            (or handle-after
                                (->| (minifn [&state :after &level &ns &data])
                                     (apply|
                                       (chain| (handling-case-execution-output|
                                                 &execute)
                                               &report))))))]
      (handle-before        state level ns data)
      (let [run-f    (handling-case-specific-context| nil
                       (handling-config-from-meta|
                         fetch-case-config-before
                         (handling-case-execution-output| &run)))
            rpt-data (run-f state level ns data)]
        (handle-after       state level ns rpt-data)))))

(defn handling-fail-fast| [f]
  (fn [s l n d]
    (if (= l :case)
      (orchestrate-level
        s l n d
        :handle-after
        (fn [s l n d]
          (let [conf (config)]
            (if (and (:fail-fast conf)
                     (some-> d :status #{:error :failure}))
              (fail-fast! s l n d)
              (let-testing-fns conf
                ((chain| (handling-case-execution-output|
                                                    &execute)
                                                  &report)
                             s :after l n d))))))
      (f s l n d))))

(def orchestrate
  (outside-in->>
    handling-captured-bindings|
    handling-captured-config|
    (with-context| {:ns    &ns
                    :level &level
                    :lang  (or (-> (context) :lang)
                               (macros/case  :clj :clj  :cljs :cljs))})
    handling-bindings-from-config
    binding-test-output|
    ; handling-fail-fast|
    orchestrate-level))

(defn run-execute-report!
  ([level ns->tsts]
   (run-execute-report! level nil ns->tsts))
  ([level ns data]
   (let [orchestrate-fn (-> (config) :orchestrate-fn)]
     (ensuring-testing-state (orchestrate-fn *testing-state* level ns data)))))
