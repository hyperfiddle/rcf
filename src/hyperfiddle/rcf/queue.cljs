(ns hyperfiddle.rcf.queue
  (:require [hyperfiddle.rcf.time :as time]))

(defprotocol IObservableQueue
  (put! [this val])
  (take! [this])
  (-empty? [this])
  (observe! [this callback])
  (unobserve! [this callback]))

(deftype ObservableArray [^js arr, observers]
  IObservableQueue
  (put! [this val]
    (if-let [observer (.shift observers)]
      (observer val)
      (.push arr val))
    this)
  (take! [_this] (.shift arr))
  (-empty? [_this] (= 0 (.-length arr)))
  (observe! [_this callback] (.push observers callback))
  (unobserve! [_this callback]
    (let [idx (.indexOf observers callback)]
      (when (> idx -1)
        (.splice observers idx 1)))))

(defn queue [] (ObservableArray. #js [] #js []))

(defn get-queue [^js q]
  (seq (.-arr q)))

(defn poll! 
  ([_ _ _ _] (throw (ex-info "Blocking poll not available on a JS runtime." {})))
  ([^js q start timeout missing-value callback]
   (let [now (time/current-time)]
     (if (time/timeout? now start timeout)
       (callback missing-value)
       (let [resolved? (volatile! false)
             resolve   (fn [val] (when-not @resolved?
                                   (vreset! resolved? true)
                                   (callback val)))]
         (if (-empty? q)
           (do (observe! q resolve)
               (js/setTimeout (fn []
                                (unobserve! q resolve)
                                (resolve missing-value))
                              (time/remaining now start timeout)))
           (resolve (take! q))))))))

(defn poll-n! [^js q start timeout missing-value n callback]
  (assert (nat-int? n))
  (poll! q start timeout missing-value
         (fn [x]
           (if (= 1 n)
             (callback x)
             (poll-n! q start timeout missing-value (dec n) (partial callback x))))))

(defn offer! [q v]
  (put! q v)
  v)
