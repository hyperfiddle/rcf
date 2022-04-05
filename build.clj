(ns build
  (:require [org.corfield.build :as bb]))

(def defaults
  {:src-pom "pom.xml"
   :lib 'com.hyperfiddle/rcf
   :version "20220405"})

(defn clean [opts]
  (bb/clean opts))

(defn jar [opts]
  (bb/jar (merge defaults opts)))

(defn install [opts]
  (bb/install (merge defaults opts)))

(defn deploy [opts]
  (bb/deploy (merge defaults opts)))