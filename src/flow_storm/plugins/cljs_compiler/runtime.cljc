(ns flow-storm.plugins.cljs-compiler.runtime
  (:require [flow-storm.runtime.indexes.api :as ia]
            [flow-storm.runtime.debuggers-api :as dbg-api]
            [flow-storm.runtime.values :as rt-values]))

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

(defn node-id [entry]
  (ia/entry-idx entry))

(defn extract-analysis-graph* [flow-id thread-id comp-timeline *nodes *edges entry parent-node-id]
  (when entry
    (let [node-id (node-id entry)
          interesting-node (cond
                             
                             (and (ia/fn-call-trace? entry)
                                  (= "cljs.analyzer" (ia/get-fn-ns entry))
                                  (= "analyze*" (ia/get-fn-name entry)))
                             {:type :analysis
                              :root? (nil? parent-node-id)
                              :node-id node-id
                              :thread-id thread-id
                              :idx (ia/entry-idx entry)
                              :form-prev (pr-str (get (ia/get-fn-args entry) 1))
                              :fn-args-ref (rt-values/reference-value! (ia/get-fn-args entry))}

                             (and (ia/fn-call-trace? entry)
                                  (= "cljs.analyzer" (ia/get-fn-ns entry))
                                  (= "parse" (ia/get-fn-name entry)))
                             {:type :parsing
                              :node-id node-id
                              :thread-id thread-id
                              :idx (ia/entry-idx entry)
                              :form-prev (pr-str (get (ia/get-fn-args entry) 2))
                              :fn-args-ref (rt-values/reference-value! (ia/get-fn-args entry))}

                             :else
                             nil)
          
          interesting-node (when interesting-node
                             (let [fn-end (get comp-timeline (ia/get-fn-ret-idx entry))]
                               (cond
                                 (ia/fn-return-trace? fn-end)
                                 (assoc interesting-node :ret-ref (rt-values/reference-value! (ia/get-expr-val fn-end)))

                                 (ia/fn-unwind-trace? fn-end)
                                 (assoc interesting-node :throwable-ref (rt-values/reference-value! (ia/get-throwable fn-end))))))]
      (when interesting-node
        (swap! *nodes assoc node-id interesting-node)
        (when parent-node-id
          (swap! *edges (fn [edgs] (update edgs parent-node-id conj node-id)))))
      
      (doseq [[_ _ cidx] (ia/callstack-node-childs flow-id thread-id (ia/entry-idx entry))]
        (let [child-entry (get comp-timeline cidx)]
          (extract-analysis-graph* flow-id thread-id comp-timeline *nodes *edges child-entry (or (:node-id interesting-node)
                                                                                                parent-node-id)))))))

(defn extract-analysis-graph [flow-id thread-id from-form]
  (let [comp-timeline (ia/get-timeline flow-id thread-id)
        root-analysis-fn-call (some (fn [entry]
                                      (when (and (ia/fn-call-trace? entry)
                                                 (= "cljs.analyzer" (ia/get-fn-ns entry))
                                                 (= "analyze*" (ia/get-fn-name entry))
                                                 (if from-form
                                                   (= from-form (get (ia/get-fn-args entry) 1))
                                                   true))
                                        entry))
                                    comp-timeline)
        *nodes (atom {})
        *edges (atom {})]
    
    (extract-analysis-graph* flow-id thread-id comp-timeline *nodes *edges root-analysis-fn-call nil)
    {:nodes @*nodes
     :edges @*edges}))

(comment
  (def at (extract-analysis-graph 0 27 '(defn sum [a b] (+ a b))))
  (def at (extract-analysis-graph 0 27 nil))
  (graph->nested-tree at))

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
                (assoc hl-entries
                       :read-ret (dbg-api/reference-timeline-entry! (ia/as-immutable entry))
                       :read-form (ia/get-expr-val entry))

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

(defn extract-compilation-graphs [flow-id {:keys [exclude-repl-wrapping?] :as opts}]
  (let [comp-timeline (get-compilation-timeline flow-id)
        {:keys [read-ret repl-wrapping-ret analysis-ret emission-ret read-form] :as hl-entries}
        (find-high-level-entries comp-timeline)]

    (when-not (and read-ret repl-wrapping-ret analysis-ret emission-ret)
      (throw (ex-info "Couldn't find all high level entries" {:hl-entries hl-entries})))
    
    {:high-level-graph {:nodes {:read-ret {:node-id :read-ret
                                           :entry read-ret
                                           :root? true}
                                :repl-wrapping-ret {:node-id :repl-wrapping-ret
                                                    :entry repl-wrapping-ret}
                                :analysis-ret {:node-id :analysis-ret
                                               :entry analysis-ret}
                                :emission-ret {:node-id :emission-ret
                                               :entry emission-ret}
                                :output {:node-id :output}}
                        :edges {:read-ret [:repl-wrapping-ret]
                                :repl-wrapping-ret [:analysis-ret]
                                :analysis-ret [:emission-ret]
                                :emission-ret [:output]
                                :output []}} 
     :analysis-graph (extract-analysis-graph 0 27 (when exclude-repl-wrapping? read-form))}))




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
(dbg-api/register-api-function :plugins.cljs-compiler/extract-compilation-graphs extract-compilation-graphs)
