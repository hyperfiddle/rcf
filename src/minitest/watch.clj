(ns minitest.watch
  (:require [clojure.set                        :as    set]
            [clojure.tools.namespace.track      :as    track]
            [clojure.tools.namespace.dependency :as    deps]
            [clojure.tools.namespace.dir        :as    dir]
            [clojure.tools.namespace.file       :as    file]
            [clojure.tools.namespace.find       :as    find]
            [clojure.tools.namespace.parse      :as    parse]
            [clojure.tools.namespace.reload     :as    load ]
            [clojure.tools.namespace.repl       :as    repl]
            [clojure.java.classpath             :refer [classpath-directories]]
            [clojure.pprint                     :refer [pprint]]
            [hawk.core                          :as     hawk]
            [minitest.config                    :refer [config with-context]]))

(repl/disable-reload!)

(defonce refresh-tracker (track/tracker))

(def referred               #'repl/referred)
(def aliased                #'repl/aliased)
(def remove-disabled        #'repl/remove-disabled)
(def print-pending-reloads  #'repl/print-pending-reloads)
(def print-and-return       #'repl/print-and-return)
(def recover-ns             #'repl/recover-ns)

(def find-files             #'dir/find-files)

(def watched-dirs (classpath-directories))

;; Adapted from https://github.com/clojure/tools.namespace/blob/0ce6646bb59a319a864faad5d01f1bc31c79e932/src/main/clojure/clojure/tools/namespace/repl.clj#L83-L109
(defn- refresh-for-tests []
  (let [current-ns-name    (ns-name *ns*)
        current-ns-refers  (referred *ns*)
        current-ns-aliases (aliased *ns*)
        tracker            (dir/scan-files
                             (track/tracker)
                             (find-files watched-dirs find/clj))
        deps               (-> tracker ::track/deps deps/map->MapDependencyGraph)
        files              (set (find-files watched-dirs find/clj)) ;; TODO: lang
        file->ns-name      (::file/filemap tracker)
        ns-name->file      (set/map-invert file->ns-name)
        files-to-reload    (fn [f]
                             (->> (map file->ns-name files)
                                  (f deps)
                                  (map ns-name->file)
                                  (filter (::dir/files tracker))
                                  set
                                  (set/union files)))
        refresh?           (some #(with-context {:ns (file->ns-name %)}
                                    (println (file->ns-name %) (-> (config) :watch))
                                    (-> (config) :watch))
                                 files)]
    (when refresh?
      (alter-var-root
          #'refresh-tracker dir/scan-files
          (files-to-reload deps/transitive-dependents-set)
          ; (set/union (files-to-reload deps/transitive-dependencies-set)
          ;            (files-to-reload deps/transitive-dependents-set))
          {:platform find/clj})
        (alter-var-root #'refresh-tracker remove-disabled)
        (print-pending-reloads refresh-tracker)
        (alter-var-root #'refresh-tracker load/track-reload)
        ; (in-ns current-ns-name)
        (let [result (print-and-return refresh-tracker)]
          (if (= :ok result)
            result
            ;; There was an error, recover as much as we can:
            (do (when-not
                  (or (false? (:clojure.tools.namespace.repl/unload (meta *ns*)))
                      (false? (:clojure.tools.namespace.repl/load   (meta *ns*))))
                  (recover-ns current-ns-refers current-ns-aliases))
                ;; Return the Exception to the REPL:
                result))))))

; (hawk/watch!
;   [{:paths watched-dirs
;     :handler (fn [_ctx _e]
;                (refresh-for-tests))}])
