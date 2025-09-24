(ns flow-storm.plugins.cljs-compiler.runtime
  (:require [flow-storm.runtime.indexes.api :as ia]
            [flow-storm.runtime.debuggers-api :as dbg-api]
            [flow-storm.runtime.values :as rt-values]
            [cljs.vendor.clojure.tools.reader.reader-types :as reader-types])
  (:import [java.io StringWriter]
           [java.lang StringBuilder]
           [cljs.vendor.clojure.tools.reader.reader_types SourceLoggingPushbackReader]))


(extend-protocol rt-values/SnapshotP
  StringWriter
  (snapshot-value [^StringWriter sw]
    {:ref/type "java.io.StringWriter"
     :ref/buffer-snapshot (.toString sw)}))

(extend-protocol rt-values/SnapshotP
  StringBuilder
  (snapshot-value [^StringBuilder sb]
    {:ref/type "java.lang.StringBuilder"
     :ref/buffer-snapshot (.toString sb)}))

(extend-protocol rt-values/SnapshotP
  SourceLoggingPushbackReader
  (snapshot-value [^SourceLoggingPushbackReader reader]
    {:ref/type "cljs.vendor.clojure.tools.reader.reader_types.SourceLoggingPushbackReader"
     :line (reader-types/get-line-number reader)
     :column (reader-types/get-column-number reader)
     :file-name (reader-types/get-file-name reader)}))

(defn ensure-indexes [{:keys [fn-call-idx] :as immutable-entry} idx]  
  (assoc immutable-entry
         :idx idx
         :fn-call-idx (or fn-call-idx idx)))

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

(defn- get-binding-val [fn-call symb-name]
  (let [binds (ia/get-fn-bindings fn-call)]
    (some (fn [b]
            (when (= symb-name (ia/get-bind-sym-name b))
              (ia/get-bind-val b)))
          binds)))

(defn extract-analysis-graph [comp-timeline root-analyze*-fn-call-idx read-form]
  (let [first-fn-call (get comp-timeline root-analyze*-fn-call-idx)
        from-idx root-analyze*-fn-call-idx
        to-idx (ia/get-fn-ret-idx first-fn-call)
        step-fn (fn [{:keys [analyzing-read-form-idx parent-stack] :as acc} entry-idx]
                  (if (empty? parent-stack)
                    (reduced acc)
                    
                    (let [entry (get comp-timeline entry-idx)
                          node-id entry-idx
                          root? (= 1 (count parent-stack))
                          start-read-form-analysis-node? (and (ia/fn-call-trace? entry)
                                                              (= "cljs.analyzer" (ia/get-fn-ns entry))
                                                              (= "analyze*" (ia/get-fn-name entry))
                                                              (= read-form (get (ia/get-fn-args entry) 1))) 
                          interesting-node (cond
                                             
                                             (and (ia/fn-call-trace? entry)
                                                  (= "cljs.analyzer" (ia/get-fn-ns entry))
                                                  (= "analyze*" (ia/get-fn-name entry)))
                                             {:type :analysis
                                              :root? root?
                                              :node-id node-id
                                              :read-form-analysis? (boolean analyzing-read-form-idx)
                                              :fn-call-idx entry-idx
                                              :form-prev (pr-str (get (ia/get-fn-args entry) 1))
                                              :fn-args-ref (rt-values/reference-value! (ia/get-fn-args entry))}

                                             (and (ia/fn-call-trace? entry)
                                                  (= "cljs.analyzer" (ia/get-fn-ns entry))
                                                  (= "parse" (ia/get-fn-name entry)))
                                             {:type :parsing
                                              :node-id node-id
                                              :fn-call-idx entry-idx
                                              :read-form-analysis? (boolean analyzing-read-form-idx)
                                              :form-prev (pr-str (get (ia/get-fn-args entry) 2))
                                              :fn-args-ref (rt-values/reference-value! (ia/get-fn-args entry))}
                                             
                                             :else
                                             nil)
                          
                          interesting-node (when interesting-node
                                             (let [end-idx (ia/get-fn-ret-idx entry)
                                                   fn-end (get comp-timeline end-idx)]
                                               (cond
                                                 (ia/fn-return-trace? fn-end)
                                                 (assoc interesting-node
                                                        :ret-ref (rt-values/reference-value! (ia/get-expr-val fn-end))
                                                        :fn-ret-idx end-idx)

                                                 (ia/fn-unwind-trace? fn-end)
                                                 (assoc interesting-node
                                                        :throwable-ref (rt-values/reference-value! (ia/get-throwable fn-end))
                                                        :fn-ret-idx end-idx))))
                          [node-id-pass pass-data] (when (and (ia/expr-trace? entry)
                                                              (= '(pass env ast opts) (ia/get-sub-form comp-timeline entry-idx)))
                                                     (let [pass-res-val (ia/get-expr-val entry)
                                                           pass-wrap-anon-fn (ia/get-fn-call comp-timeline entry-idx)
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
                          true                           (assoc-in [:nodes node-id] interesting-node)
                          true                           (update :parent-stack conj node-id)
                          (not root?)                    (update-in [:edges (first parent-stack)] conj node-id)
                          start-read-form-analysis-node? (assoc :analyzing-read-form-idx entry-idx))  

                        pass-data
                        (update-in acc [:nodes node-id-pass :passes] (fnil conj []) pass-data)

                        (and (ia/fn-end-trace? entry)
                             (= (ia/fn-call-idx entry) analyzing-read-form-idx))
                        (assoc acc
                               :analyzing-read-form-idx nil
                               :read-form-ast (ia/get-expr-val entry))
                        
                        (and (ia/fn-end-trace? entry)
                             (= (ia/fn-call-idx entry) (first parent-stack)))
                        (update acc :parent-stack pop)

                        :else
                        acc))))]
    (reduce step-fn
            {:parent-stack (list from-idx)
             :analyzing-read-form-idx nil
             :read-form-ast nil
             :nodes {}
             :edges {}}
            (range from-idx (inc to-idx)))))

(defn extract-emission-graph [comp-timeline root-emit-str-fn-call-idx read-form-ast]
  (let [first-fn-call (get comp-timeline root-emit-str-fn-call-idx)
        from-idx root-emit-str-fn-call-idx
        to-idx (ia/get-fn-ret-idx first-fn-call)
        step-fn (fn [{:keys [emitting-read-form-idx parent-stack] :as acc} entry-idx]
                  (if (empty? parent-stack)
                    (reduced acc)
                    
                    (let [entry (get comp-timeline entry-idx)
                          node-id entry-idx
                          start-read-form-emission-node? (and (ia/fn-call-trace? entry)
                                                              (= "cljs.compiler" (ia/get-fn-ns entry))
                                                              (= "emit" (ia/get-fn-name entry))
                                                              (= (:form read-form-ast) (:form (get (ia/get-fn-args entry) 0))))]
                      (cond                      
                        (and (ia/fn-call-trace? entry)
                             (= "cljs.compiler" (ia/get-fn-ns entry))
                             (= "emit" (ia/get-fn-name entry)))
                        (cond-> acc
                          true (assoc-in [:nodes node-id] {:type :emission
                                                           :node-id node-id
                                                           :read-form-emission? (boolean emitting-read-form-idx)
                                                           :fn-call-idx entry-idx
                                                           :ast-op (-> (ia/get-fn-args entry) first :op)
                                                           :fn-args-ref (rt-values/reference-value! (ia/get-fn-args entry))})
                          true                           (update :parent-stack conj node-id)                            
                          true                           (update-in [:edges (first parent-stack)] conj node-id)
                          start-read-form-emission-node? (assoc :emitting-read-form-idx entry-idx))

                        (and (ia/expr-trace? entry)
                             (= '*out* (ia/get-sub-form comp-timeline entry-idx))
                             (let [fn-call (ia/get-fn-call comp-timeline entry-idx)]
                               (and (= "cljs.compiler" (ia/get-fn-ns fn-call))
                                    (= "emits" (ia/get-fn-name fn-call)))))
                        (let [out-s (ia/get-expr-val (get comp-timeline (inc entry-idx)))
                              write-out-node-id (first parent-stack)]
                          (update-in acc [:nodes write-out-node-id :write-outs] (fnil conj []) {:write-out out-s
                                                                                                :idx entry-idx}))

                        (and (ia/fn-end-trace? entry)
                             (= (ia/fn-call-idx entry) emitting-read-form-idx))
                        (assoc acc :emitting-read-form-idx nil)
                        
                        (and (ia/fn-end-trace? entry)
                             (= (ia/fn-call-idx entry) (first parent-stack)))
                        (update acc :parent-stack pop)

                        :else
                        acc))))]
    (reduce step-fn
            {:parent-stack (list from-idx)
             :emitting-read-form-idx nil
             :nodes {root-emit-str-fn-call-idx {:type :emission
                                                :root? true
                                                :node-id root-emit-str-fn-call-idx
                                                :idx root-emit-str-fn-call-idx
                                                :fn-args-ref (rt-values/reference-value! (ia/get-fn-args first-fn-call))}}
             :edges {}}
            (range from-idx (inc to-idx)))))


(defn- immutable-reference-entry [entry entry-idx]
  (-> entry
      ia/as-immutable
      (ensure-indexes entry-idx)
      dbg-api/reference-timeline-entry!))

(defn find-high-level-entries [comp-timeline _opts]
  (reduce (fn [{:keys [analysis emission entry-idx] :as hl-entries} entry]
            (if (every? identity (vals hl-entries))
              (reduced hl-entries)

              (let [hl-entries' (cond
                                  ;; read-ret
                                  (and (ia/fn-return-trace? entry)
                                       (let [fn-call (ia/get-fn-call comp-timeline entry-idx)]
                                         (and (= "cljs.vendor.clojure.tools.reader" (ia/get-fn-ns fn-call))
                                              (= "read" (ia/get-fn-name fn-call)))))
                                  (assoc hl-entries
                                         :read-ret (immutable-reference-entry entry entry-idx)
                                         :read-form (ia/get-expr-val entry))

                                  ;; repl-wrapping-ret
                                  (and (ia/expr-trace? entry)
                                       (= '(wrap form) (ia/get-sub-form comp-timeline entry-idx)))
                                  (assoc hl-entries :repl-wrapping-ret (immutable-reference-entry entry entry-idx))

                                  ;; analysis
                                  (and (nil? analysis)
                                       (ia/fn-call-trace? entry)
                                       (= "cljs.analyzer" (ia/get-fn-ns entry))
                                       (= "analyze*" (ia/get-fn-name entry)))
                                  (let [ret-idx (ia/get-fn-ret-idx entry)
                                        fn-ret (get comp-timeline ret-idx)]
                                    (assoc hl-entries :analysis {:fn-call   (immutable-reference-entry entry entry-idx)
                                                                 :fn-return (immutable-reference-entry fn-ret ret-idx)}))

                                  ;; emission
                                  (and (nil? emission)
                                       (ia/fn-call-trace? entry)
                                       (= "cljs.compiler" (ia/get-fn-ns entry))
                                       (= "emit-str" (ia/get-fn-name entry)))
                                  (let [ret-idx (ia/get-fn-ret-idx entry)
                                        fn-ret (get comp-timeline ret-idx)]                  
                                    (assoc hl-entries :emission {:fn-call   (immutable-reference-entry entry entry-idx)
                                                                 :fn-return (immutable-reference-entry fn-ret ret-idx)}))
                                  
                                  :else hl-entries)]
                (update hl-entries' :entry-idx inc))))
          {:read-ret nil
           :read-form nil
           :repl-wrapping-ret nil
           :analysis nil
           :emission nil
           :entry-idx 0}
          comp-timeline))

(defn extract-compilation-graphs [flow-id opts]
  (let [comp-timeline (get-compilation-timeline flow-id)
        {:keys [read-ret read-form repl-wrapping-ret analysis emission] :as hl-entries}
        (find-high-level-entries comp-timeline opts)]

    (when-not (and read-ret repl-wrapping-ret analysis emission)
      (throw (ex-info "Couldn't find all high level entries" {:hl-entries hl-entries})))

    (let [{:keys [read-form-ast] :as analysis-graph} (extract-analysis-graph comp-timeline (-> analysis :fn-call :idx) read-form)
          emission-graph (extract-emission-graph comp-timeline (-> emission :fn-call :idx) read-form-ast)]
      {:thread-id (ia/timeline-thread-id comp-timeline nil)
       :flow-id flow-id
       :high-level-graph {:nodes {:read-ret {:node-id :read-ret
                                             :data read-ret
                                             :root? true}
                                  :repl-wrapping-ret {:node-id :repl-wrapping-ret
                                                      :data repl-wrapping-ret}
                                  :analysis {:node-id :analysis
                                             :data analysis}
                                  :emission {:node-id :emission
                                             :data emission}}
                          :edges {:read-ret [:repl-wrapping-ret]
                                  :repl-wrapping-ret [:analysis]
                                  :analysis [:emission]
                                  :emission []}} 
       :analysis-graph (select-keys analysis-graph [:nodes :edges])
       :emission-graph (select-keys emission-graph [:nodes :edges])})))

(dbg-api/register-api-function :plugins.cljs-compiler/extract-compilation-graphs extract-compilation-graphs)


(comment
  (extract-compilation-graphs 0 nil)
  )
