
(defn- symbol-ns-selector [sym]
  (let [s          (name sym)
        [neg? nme] (if (-> s first (= \!))
                     [true  (subs s 1)]
                     [false s])]
    ((if neg? complement identity)
     #{nme})))


(defn- regex? [x]
  (instance? java.util.regex.Pattern x))

(defn- regex-ns-selector [reg]
  (partial re-matches reg))


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
    symbol?  (symbol-ns-selector x)
    regex?   (regex-ns-selector  x)
    string?  (glob-ns-selector   x)
    #{:all}  (constantly true)
    ifn?     x))

(defn- parse-selectors [selectors]
  (let [[sels & more] (split-with #{:exclude} selectors)
        [excl more]   (if (-> more first #{:exclude})
                        [(second more) (drop 2 more)]
                        [nil           more])
        sels          (map ns-selector sels)]
    (concat sels
            (map (->| ns-selector complement) excl)
            (parse-selectors more))))
