(ns minitest.utils
  (:require [net.cgrand.macrovich        :as    macros]
            [clojure.walk                :refer [postwalk]]
            [camel-snake-kebab.core      :refer [->kebab-case]])
 #?(:cljs
     (:require-macros [minitest.utils :refer [mcall]])))

(def ->|               #(apply comp (reverse %&)))
(def call              #(apply %1 %&))
(def as-form           #(do `'~%))
(def as-thunk          #(do `(fn [] ~%)))
(def as-wildcard-thunk #(do `(fn [] (let [~'_ '~'_] ~%))))

(macros/deftime
  (defmacro current-file []
    (if (-> *file* first (= \/))
      *file*
      (-> *file* ClassLoader/getSystemResource .getPath)))

  (defmacro current-ns-name []
    `'~(ns-name *ns*))

  ;; Taken from:
  ;; https://github.com/ptaoussanis/encore/blob/009da50b8d38ebd00db0ec99bd99312cf0c56203/src/taoensso/encore.cljc#L374
  ;; TODO: keep ?
  (defmacro defalias
    "Defines an alias for a var, preserving its metadata."
    ([    src      ] `(defalias ~(symbol (name src)) ~src nil))
    ([sym src      ] `(defalias ~sym                 ~src nil))
    ([sym src attrs]
     (let [attrs (if (string? attrs) {:doc attrs} attrs)] ; Back compatibility
       `(let [attrs# (conj (select-keys (meta (var ~src))
                                        [:doc :arglists :private :macro])
                           ~attrs)]
          (alter-meta! (def ~sym @(var ~src)) conj attrs#)
          (var ~sym))))))

;; Taken from https://stackoverflow.com/questions/14488150/how-to-write-a-dissoc-in-command-for-clojure
(defn dissoc-in [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

;; Taken from:
;; https://github.com/TristeFigure/shuriken/blob/master/src/shuriken/sequential.clj
(defn slice
  "Slice a seq using a delimiter predicate. There are two options:
  ```
  - :include-delimiter  false | :left | :right
                          whether to include the delimiter and where
  - :include-empty      true | false
                          whether to create empty seqs between
                          successive delimiters
  ```

  ```clojure
  (let [coll [1 1 0 1 0 0 1 1]]
    ;; the default
    (slice zero? coll) ;; by default, :include-delimiter false,
                                      :include-empty     false
    => ((1 1) (1) (1 1))

    (slice zero? coll :include-empty true)
    => ((1 1) (1) () (1 1))

    (slice zero? coll :include-delimiter :left)
    => ((1 1) (0 1) (0 1 1))

    (slice zero? coll :include-delimiter :right)
    => ((1 1 0) (1 0) (1 1))

    (slice zero? coll :include-delimiter :right :include-empty true)
    => ((1 1 0) (1 0) (0) (1 1))
    )
  ```"
  [delimiter? coll & {:keys [include-delimiter include-empty]
                      :or {include-delimiter false
                           include-empty     false}}]
  (let [not-delimiter? (complement delimiter?)
        [before-first-delim from-first-delim] (split-with not-delimiter? coll)
        result
        (loop [xs from-first-delim
               acc []]
          (if (empty? xs)
            acc
            (let [delim (first xs)
                  [after-delim next-slice] (split-with not-delimiter?
                                                       (rest xs))]
              (recur
                next-slice
                (if (and (not include-empty)
                         (empty? after-delim))
                  acc
                  (let [current-slice (case include-delimiter
                                        (nil false) after-delim
                                        :left (cons delim after-delim)
                                        :right (concat after-delim
                                                       (take 1 next-slice)))]
                    (conj acc current-slice)))))))]
    (seq (if (empty? before-first-delim)
           result
           (cons (if (= include-delimiter :right)
                   (concat before-first-delim (take 1 from-first-delim))
                   before-first-delim)
                 result)))))
