(ns minitest.ns
  (:require [net.cgrand.macrovich         :as    macros]
    #?(:clj [clojure.java.io              :as    io])
    #?(:clj [clojure.tools.namespace.find :as    tools])
    #?(:clj [minitest.config              :refer [config]])
            [minitest.utils               :refer [call ->|]]))

(macros/deftime
  (defmacro find-test-namespaces []
    (let [dirs (-> (config) :dirs set)]
      `'~(set (mapcat #(tools/find-namespaces-in-dir
                         (io/file %)
                         (macros/case
                           :clj  tools/clj
                           :cljs tools/cljs))
                      dirs)))))

(defn- regex? [x]
  (instance? (macros/case
               :clj  java.util.regex.Pattern
               :cljs js/RegExp)
             x))

(defn- regex-ns-selector [reg]
  #(->> % name (re-matches reg)))

;; Taken from: https://github.com/jkk/clj-glob/blob/master/src/org/satta/glob.clj
;; TODO: acknowledge
(defn- glob->regex [s]
  (loop [stream      s
         re          ""
         curly-depth 0]
    (let [[c j] stream]
      (cond
        (nil? c) (re-pattern (str (if (= \. (first s)) "" "(?=[^\\.])") re))
        (= c \\) (recur (nnext stream) (str re c c) curly-depth)
        (= c \/) (recur (next stream) (str re (if (= \. j) c "/(?=[^\\.])"))
                        curly-depth)
        (= c \*) (recur (next stream) (str re "[^/]*") curly-depth)
        (= c \?) (recur (next stream) (str re "[^/]") curly-depth)
        (= c \{) (recur (next stream) (str re \() (inc curly-depth))
                        (= c \}) (recur (next stream) (str re \)) (dec curly-depth))
        (and (= c \,) (< 0 curly-depth))  (recur (next stream) (str re \|)
                                                 curly-depth)
        (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur (next stream) (str re \\ c)
                                                 curly-depth)
        :else (recur (next stream) (str re c) curly-depth)))))


(defn- glob-ns-selector [glob]
  (regex-ns-selector (glob->regex glob)))

(defn- ns-selector [x]
  (condp call x
    string?     (glob-ns-selector x)
    symbol?     (glob-ns-selector (name x))
    regex?      (regex-ns-selector x)
    #{:all}     (constantly true)
    coll?       (apply some-fn (map ns-selector x))
    ifn?        x))

(defn- exclude| [f]
  (with-meta (->| f #(if % :exclude %))
    {::exclude true}))

(defn parse-selectors [data]
  (let [parse
        (fn parse-them [[x & more]]
          (when x
            (let [[sel even-more]
                  (case x
                    :exclude  [(exclude| (ns-selector (first more))) (rest more)]
                    :all      [(ns-selector x)                       more]
                    (do       [(ns-selector x)                       more]))]
              (cons sel (parse-them even-more)))))
        sels (parse data)]
    ;; if all the selectors are exclusions, all the nss will be considered.
    (if (every? (->| meta ::exclude) sels)
      (cons (ns-selector :all) sels)
      sels)))
