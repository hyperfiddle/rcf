Build
```shell
HYPERFIDDLE_RCF_DATE=`date -u '+%Y%m%d-%H%M%S'`
git tag v$HYPERFIDDLE_RCF_DATE
clojure -T:build clean
clojure -T:build jar :version '"'$HYPERFIDDLE_RCF_DATE'"'
clojure -T:build install :version '"'$HYPERFIDDLE_RCF_DATE'"'
```

Test local maven repo:
```clojure
{:deps    {org.clojure/clojure {:mvn/version "1.10.3"}
           com.hyperfiddle/rcf {:mvn/version $HYPERFIDDLE_RCF_DATE}}}
```

# echo clj :replace-deps '{:deps {com.hyperfiddle/photon {:mvn/version "'$HYPERFIDDLE_RCF_DATE'"}}}'

Deploy
```shell
CLOJARS_PASSWORD= \
CLOJARS_USERNAME=dustingetz \
clojure -T:build deploy :version '"'$HYPERFIDDLE_RCF_DATE'"'
```
