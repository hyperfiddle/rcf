(ns hyperfiddle.rcf.utils
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn pprint-str [x]
  (with-out-str (pprint/pprint x)))

(defn pprint [x]
  (print (str/trimr (pprint-str x))))

(defn testing-vars-str [m]
  (let [{:keys [file line]} m]
    (str file ":" line )))

(defn cleanup [form]
  (walk/postwalk (fn [x]
                   (cond
                     (and (symbol? x)
                          (= "hyperfiddle.rcf" (namespace x))) (symbol (name x))
                     (and (sequential? x)
                          (= 'quote (first x)))                (second x)
                     :else                                     x))
                 form))

(defn extract-comparison [m]
  (let [{:keys [expected actual]} m
        expected                  (cleanup expected)
        actual                    (cleanup actual)]
    (cond
      (and (sequential? expected)
           (= 'unifies? (first expected))) (rest expected)
      (sequential? actual)                 (rest actual)
      :else                                [expected actual])))
