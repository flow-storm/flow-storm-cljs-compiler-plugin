(ns flow-storm.plugins.cljs-compiler.runtime
  (:require [flow-storm.runtime.indexes.api :as ia]
            [flow-storm.runtime.debuggers-api :as dbg-api]))

(defn get-compilation-timeline
  "Return the first timeline that recorded an entry with a
  function call to cljs.repl/eval-cljs"
  [flow-id]
  (some (fn [thread-id]
          (let [tl (ia/get-timeline flow-id thread-id)]
            (when (some (fn [entry]
                          (and (ia/fn-call-trace? entry)
                               (= "cljs.repl" (ia/get-fn-ns entry))
                               (= "eval-cljs" (ia/get-fn-name entry))))
                        tl)
              tl)))    
        (ia/all-threads-ids flow-id)))

(defn find-high-level-entries [comp-timeline]
  (reduce (fn [hl-entries entry]
            (if (every? identity (vals hl-entries))
              (reduced hl-entries)

              (cond
                ;; read-ret
                (and (ia/fn-return-trace? entry)
                     (let [fn-call (ia/get-fn-call comp-timeline entry)]
                       (= "cljs.vendor.clojure.tools.reader" (ia/get-fn-ns fn-call))
                       (= "read" (ia/get-fn-name fn-call))))
                (assoc hl-entries :read-ret (dbg-api/reference-timeline-entry! (ia/as-immutable entry)))

                ;; repl-wrapping-ret
                (and (ia/expr-trace? entry)
                     (= '(wrap form) (ia/get-sub-form comp-timeline entry)))
                (assoc hl-entries :repl-wrapping-ret (dbg-api/reference-timeline-entry! (ia/as-immutable entry)))

                ;; analysis-ret
                (and (ia/expr-trace? entry)
                     (= '(->ast form) (ia/get-sub-form comp-timeline entry))
                     (let [fn-call (ia/get-fn-call comp-timeline entry)]
                       (= "cljs.repl" (ia/get-fn-ns fn-call))
                       (= "evaluate-form" (ia/get-fn-name fn-call))))
                (assoc hl-entries :analysis-ret (dbg-api/reference-timeline-entry! (ia/as-immutable entry)))

                ;; emission-ret
                (and (ia/expr-trace? entry)
                     (= '(comp/emit-str ast) (ia/get-sub-form comp-timeline entry))
                     (let [fn-call (ia/get-fn-call comp-timeline entry)]
                       (= "cljs.repl" (ia/get-fn-ns fn-call))
                       (= "evaluate-form" (ia/get-fn-name fn-call))))
                (assoc hl-entries :emission-ret (dbg-api/reference-timeline-entry! (ia/as-immutable entry)))
                
                :else hl-entries)))
          {:read-ret nil
           :repl-wrapping-ret nil
           :analysis-ret nil
           :emission-ret nil}
          comp-timeline))

(defn extract-compilation-tree [flow-id]
  (let [comp-timeline (get-compilation-timeline flow-id)
        {:keys [read-ret repl-wrapping-ret analysis-ret emission-ret] :as hl-entries}
        (find-high-level-entries comp-timeline)]

    (when-not (and read-ret repl-wrapping-ret analysis-ret emission-ret)
      (throw (ex-info "Couldn't find all high level entries" {:hl-entries hl-entries})))
    
    {:outer-tree {:node-id :read-ret
                  :entry read-ret
                  :childs [{:node-id :repl-wrapping-ret
                            :entry repl-wrapping-ret
                            :childs [{:node-id :analysis-ret
                                      :entry analysis-ret
                                      :childs [{:node-id :emission-ret
                                                :entry emission-ret
                                                :childs [{:node-id :output}]}]}]}]}}))




(comment

  (require '[clj-tree-layout.core :refer [layout-tree]])

  (reduce (fn [edges {:keys [node-id childs]}]
            (into edges (mapv (fn [c] [node-id (:node-id c)]) childs)))
          []
          (tree-seq :childs :childs outer-tree))
  
  (def outer-tree (:outer-tree (extract-compilation-tree 0)))
(nodes-by-id outer-tree)  
  (layout-tree
   outer-tree
   {:sizes (zipmap (mapv :node-id (tree-seq :childs :childs outer-tree))
                   (repeat [100 100]))
    :branch-fn :childs
    :childs-fn :childs
    :id-fn :node-id})
  )
(dbg-api/register-api-function :plugins.cljs-compiler/extract-compilation-tree extract-compilation-tree)
