Build
```shell
HYPERFIDDLE_RCF_DATE=`date '+%Y%m%d'` 
clojure -T:build jar :version '"'$HYPERFIDDLE_RCF_DATE'"'
git tag v$HYPERFIDDLE_RCF_DATE
```

Clean 
`clojure -T:clean`

Install jar in local maven repo:
`clojure -T:build install :version '"'$HYPERFIDDLE_RCF_DATE'"'`

Test local maven repo:
```clojure
{:deps    {org.clojure/clojure {:mvn/version "1.10.3"}
           com.hyperfiddle/rcf {:mvn/version "20220405"}}}

```

Deploy
```shell
CLOJARS_USERNAME=dustingetz CLOJARS_PASSWORD= clojure -T:build deploy :version '"'$HYPERFIDDLE_RCF_DATE'"'
```

rm local m2 cache
test again
