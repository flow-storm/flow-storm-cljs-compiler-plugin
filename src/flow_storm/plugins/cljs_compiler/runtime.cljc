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

(defn- get-binding-val [fn-call symb-name]
  (let [binds (ia/get-fn-bindings fn-call)]
    (some (fn [b]
            (when (= symb-name (ia/get-bind-sym-name b))
              (ia/get-bind-val b)))
          binds)))

(def tl (ia/get-timeline 0 27))
(def pres (get tl 41950))
(def pres-fn-call (ia/get-fn-call tl pres))
(get-binding-val pres-fn-call "pass")

(get tl (ia/get-fn-parent-idx pres-fn-call))

(defn extract-analysis-graph* [flow-id thread-id comp-timeline first-fn-call]
  (let [from-idx (ia/entry-idx first-fn-call)
        to-idx (ia/get-fn-ret-idx first-fn-call)
        step-fn (fn [{:keys [nodes edges parent-stack] :as acc} entry-idx]
                  (if (empty? parent-stack)
                    (reduced acc)
                    
                    (let [entry (get comp-timeline entry-idx)
                          node-id (node-id entry)
                          root? (= 1 (count parent-stack))
                          interesting-node (cond
                                             
                                             (and (ia/fn-call-trace? entry)
                                                  (= "cljs.analyzer" (ia/get-fn-ns entry))
                                                  (= "analyze*" (ia/get-fn-name entry)))
                                             {:type :analysis
                                              :root? root?
                                              :node-id node-id
                                              :thread-id thread-id
                                              :idx entry-idx
                                              :form-prev (pr-str (get (ia/get-fn-args entry) 1))
                                              :fn-args-ref (rt-values/reference-value! (ia/get-fn-args entry))}

                                             (and (ia/fn-call-trace? entry)
                                                  (= "cljs.analyzer" (ia/get-fn-ns entry))
                                                  (= "parse" (ia/get-fn-name entry)))
                                             {:type :parsing
                                              :node-id node-id
                                              :thread-id thread-id
                                              :idx entry-idx
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
                                                 (assoc interesting-node :throwable-ref (rt-values/reference-value! (ia/get-throwable fn-end))))))
                          [node-id-pass pass-data] (when (and (ia/expr-trace? entry)
                                                              (= '(pass env ast opts) (ia/get-sub-form comp-timeline entry)))
                                                     (let [pass-res-val (ia/get-expr-val entry)
                                                           pass-wrap-anon-fn (ia/get-fn-call comp-timeline entry)
                                                           ast-bind-val (get-binding-val pass-wrap-anon-fn "ast")
                                                           pass-bind-val (get-binding-val pass-wrap-anon-fn "pass")]
                                                       (when (not (identical? pass-res-val ast-bind-val))
                                                         ;; it means that the pass applied
                                                         (let [analyze*-node-id (ia/get-fn-parent-idx pass-wrap-anon-fn)]
                                                           ;; parent-fn-call should be the  wrapping analyze* for the passes,
                                                           ;; so its idx is its node-id
                                                           [analyze*-node-id {:pass-name (pr-str pass-bind-val)
                                                                              :idx entry-idx}]))))]

                      (cond                      
                        interesting-node
                        (cond-> acc
                          true        (assoc-in [:nodes node-id] interesting-node)
                          true        (update :parent-stack conj node-id)
                          (not root?) (update-in [:edges (first parent-stack)] conj node-id))

                        pass-data
                        (update-in acc [:nodes node-id-pass :passes] (fnil conj []) pass-data)
                        
                        (and (ia/fn-end-trace? entry)
                             (= (ia/fn-call-idx entry) (first parent-stack)))
                        (update acc :parent-stack pop)

                        :else
                        acc))))]
    (reduce step-fn
            {:parent-stack (list from-idx)
             :nodes {}
             :edges {}}
            (range from-idx (inc to-idx)))))

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
                                    comp-timeline)]
    
    (-> (extract-analysis-graph* flow-id thread-id comp-timeline  root-analysis-fn-call)
        (select-keys [:nodes :edges]))))

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
                                               :entry emission-ret}}
                        :edges {:read-ret [:repl-wrapping-ret]
                                :repl-wrapping-ret [:analysis-ret]
                                :analysis-ret [:emission-ret]
                                :emission-ret []}} 
     :analysis-graph (extract-analysis-graph 0 27 (when exclude-repl-wrapping? read-form))}))


(defn- graph->nested-tree
  ([{:keys [nodes edges] :as g}]
   (let [root (some (fn [node] (when (:root? node) node))
                    (vals nodes))]
     (graph->nested-tree g root)))

  ([{:keys [nodes edges] :as g} node]
   (let [childs (mapv (fn [nid] (graph->nested-tree g (get nodes nid))) (get edges (:node-id node)))]
     (assoc node :childs childs))))

(comment
(graph->nested-tree at)
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
