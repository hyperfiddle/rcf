
(defn- symbol-ns-selector [sym]
  (let [s          (name sym)
        [neg? nme] (if (-> s first (= \!))
                     [true  (subs s 1)]
                     [false s])]
    ((if neg? complement identity)
     (->| name #{nme}))))


(defn- regex? [x]
  (instance? java.util.regex.Pattern x))

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
    symbol?  (symbol-ns-selector x)
    regex?   (regex-ns-selector  x)
    string?  (glob-ns-selector   x)
    #{:all}  (constantly true)
    ifn?     (-> name x)))

(defn- parse-selectors [[x & more]]
  (when x
    (let [[sel even-more]
          (case x
            :exclude  [(complement (ns-selector (first more))) (rest more)]
            :all      [(ns-selector x)                         more]
            (do       [(ns-selector x)                         more]))]
      (cons sel (parse-selectors even-more)))))
