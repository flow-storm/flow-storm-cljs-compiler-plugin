(ns flow-storm.plugins.cljs-compiler.ui
  (:require [flow-storm.debugger.ui.plugins :as fs-plugins]
            [flow-storm.debugger.runtime-api :as runtime-api :refer [rt-api]]
            [flow-storm.debugger.ui.components :as ui]
            [flow-storm.debugger.ui.utils :as ui-utils]
            [flow-storm.debugger.ui.data-windows.data-windows :as data-windows]
            [clojure.string :as str]
            [clj-tree-layout.core :refer [layout-tree]])
  (:import [javafx.scene.control Label Button TextField]
           [javafx.scene.paint Paint]
           [javafx.scene.transform Scale]
           [javafx.scene.layout HBox VBox]
           [javafx.scene.shape Line Rectangle]
           [javafx.event EventHandler]
           [javafx.scene Node]))

(defn- graph->nested-tree
  ([{:keys [nodes edges] :as g}]
   (let [root (some (fn [node] (when (:root? node) node))
                    (vals nodes))]
     (graph->nested-tree g root)))

  ([{:keys [nodes edges] :as g} node]
   (let [childs (mapv (fn [nid] (graph->nested-tree g (get nodes nid))) (get edges (:node-id node)))]
     (assoc node :childs childs))))

(defn- labeled-container [& {:keys  [label x y width height]}]
  (let [rect (doto (Rectangle. 0 0 width height)
               (.setFill  (Paint/valueOf "#336699")))
        lbl (doto (ui/label :text label)
              (.setLayoutX 10)
              (.setLayoutY 10))]
    (doto (ui/pane
           :childs
           [rect lbl])
      (.setLayoutX x)
      (.setLayoutY y)
      (.setPrefWidth width)
      (.setPrefHeight height))))

(defn- labeled-node [& {:keys  [label x y width height]}]
  (doto (ui/label :text label)
    (.setLayoutX x)
    (.setLayoutY y)
    (.setPrefWidth width)
    (.setPrefHeight height)
    (.setStyle "-fx-border-color:#333; -fx-border-width: 5; -fx-background-color: pink;")))

(defn- arrow [& {:keys [from-x from-y to-x to-y on-click]}]
  (doto (Line. from-x from-y to-x to-y)
    (.setStyle "-fx-stroke-width: 5;")
    (.setOnMouseClicked
     (ui-utils/event-handler
         [mev]
       (on-click mev)))))

(defn- build-analysis-pane [{:keys [nodes edges]} node-id->layout]
  (let [nodes-vec (reduce-kv (fn [acc nid node]
                               (let [{:keys [x y width height]} (node-id->layout nid)
                                     node (nodes nid)
                                     fn-lbl (case (:type node)
                                              :analysis "Analysis"
                                              :parsing  "Parse")
                                     node-box (doto
                                                  (ui/v-box
                                                   :childs [(ui/label :text "Analyzing")
                                                            (ui/label :text (:form-prev node))])
                                                (.setLayoutX x)
                                                (.setLayoutY y)
                                                (.setPrefWidth width)
                                                (.setPrefHeight height)
                                                (.setStyle (case (:type node)
                                                             :analysis "-fx-border-color:#333; -fx-border-width: 5; -fx-background-color: #a31c32;"
                                                             :parsing  "-fx-border-color:#333; -fx-border-width: 5; -fx-background-color: #7a3e16;")))]

                                 (conj acc node-box)))
                             []
                             nodes)
        arrows-vec (reduce-kv (fn [arrows node-id-from to-ids]
                                (reduce (fn [arrs node-id-to]
                                          (let [node-from (node-id->layout node-id-from)
                                                node-to (node-id->layout node-id-to)
                                                arr-offset 50
                                                call-from-x (+ (:x node-from) (/ (:width node-from) 2))
                                                call-from-y (+ (:y node-from) (:height node-from))
                                                call-to-x   (- (+ (:x node-to) (/ (:width node-to) 2)) arr-offset)
                                                call-to-y   (:y node-to)

                                                ret-from-x (+ (:x node-to) (/ (:width node-to) 2) arr-offset)
                                                ret-from-y (:y node-to)
                                                ret-to-x   (+ (:x node-from) (/ (:width node-from) 2))
                                                ret-to-y   (+ (:y node-from) (:width node-from))

                                                node (nodes node-id-from)
                                                call-arr (arrow :from-x call-from-x
                                                                :from-y call-from-y
                                                                :to-x   call-to-x
                                                                :to-y   call-to-y
                                                                :on-click (fn [mev]
                                                                            (let [{:keys [fn-args-ref]} (nodes node-id-to)]
                                                                              (runtime-api/data-window-push-val-data rt-api
                                                                                                                     :plugins/cljs-compiler
                                                                                                                     fn-args-ref
                                                                                                                     {:flow-storm.debugger.ui.data-windows.data-windows/dw-id :plugins.cljs-compiler/extract-compilation-tree
                                                                                                                      :flow-storm.debugger.ui.data-windows.data-windows/stack-key "FnCall"
                                                                                                                      :root? true}))))
                                                ret-arr (arrow :from-x ret-from-x
                                                               :from-y ret-from-y
                                                               :to-x   ret-to-x
                                                               :to-y   ret-to-y
                                                               :on-click (fn [mev]
                                                                           (let [{:keys [ret-ref throwable-ref]} (nodes node-id-to)]
                                                                              (runtime-api/data-window-push-val-data rt-api
                                                                                                                     :plugins/cljs-compiler
                                                                                                                     (or ret-ref throwable-ref)
                                                                                                                     {:flow-storm.debugger.ui.data-windows.data-windows/dw-id :plugins.cljs-compiler/extract-compilation-tree
                                                                                                                      :flow-storm.debugger.ui.data-windows.data-windows/stack-key "FnReturn"
                                                                                                                      :root? true}))))]
                                            (into arrs [call-arr ret-arr])))
                                 arrows
                                 to-ids))
                          []
                          edges)
        nodes-and-arrows-vec (-> nodes-vec
                                (into arrows-vec))]
    (ui/pane :childs nodes-and-arrows-vec)))

(defn- build-high-level-pane [{:keys [nodes edges]} node-id->layout]
  (let [nodes-vec (reduce-kv (fn [acc nid node]
                               (let [{:keys [x y width height]} (node-id->layout nid)]
                                 (conj acc (labeled-container :label (name (:node-id node))
                                                              :x x
                                                              :y y
                                                              :width width
                                                              :height height))))
                             []
                             nodes)
        arrows-vec (reduce-kv (fn [arrows node-id-from to-ids]
                                (reduce (fn [arrs node-id-to]
                                          (let [layout-from (node-id->layout node-id-from)
                                                layout-to (node-id->layout node-id-to)
                                                from-x (+ (:x layout-from) (/ (:width layout-from) 2))
                                                from-y (+ (:y layout-from) (:height layout-from))
                                                to-x   (+ (:x layout-to) (/ (:width layout-to) 2))
                                                to-y   (:y layout-to)
                                                {:keys [entry]} (nodes node-id-from)
                                                arr (arrow :from-x from-x
                                                           :from-y from-y
                                                           :to-x   to-x
                                                           :to-y   to-y
                                                           :on-click (fn [mev]
                                                                       (prn "@@@@ clicked HL")
                                                                       (let [val-ref (case (:type entry)
                                                                                       :fn-return (:result entry)
                                                                                       :expr (:result entry))]
                                                                         (runtime-api/data-window-push-val-data rt-api
                                                                                                                :plugins/cljs-compiler
                                                                                                                val-ref
                                                                                                                {:flow-storm.debugger.ui.data-windows.data-windows/dw-id :plugins.cljs-compiler/extract-compilation-tree
                                                                                                                 :flow-storm.debugger.ui.data-windows.data-windows/stack-key "Data"
                                                                                                                 :root? true}))))]
                                            (conj arrs arr)))
                                 arrows
                                 to-ids))
                          []
                          edges)
        nodes-and-arrows-vec (-> nodes-vec
                                (into arrows-vec))]
    (ui/pane :childs nodes-and-arrows-vec)))

(defn- layout-size [layout]
  (let [{:keys [max-x max-y min-x min-y]}
        (reduce (fn [acc {:keys [x y width height]}]
                  (-> acc
                      (update :max-x max (+ x width))
                      (update :max-y max (+ y height))
                      (update :min-x min x)
                      (update :min-y min y)))
                {:max-x Long/MIN_VALUE
                 :max-y Long/MIN_VALUE
                 :min-x Long/MAX_VALUE
                 :min-y Long/MAX_VALUE}
                (vals layout))]
    {:width  (- max-x min-x)
     :height (- max-y min-y)}))

(defn- translate-layout [[x y] layout]
  (reduce-kv (fn [acc nid lay]
               (assoc acc nid (-> lay
                                  (update :x #(+ x %))
                                  (update :y #(+ y %)))))
             {}
             layout))

(defn build-diagram-pane [{:keys [high-level-graph analysis-graph]}]
  (let [analysis-layout (layout-tree
                         (graph->nested-tree analysis-graph)
                         {:sizes (zipmap (keys (:nodes analysis-graph)) (repeat [200 200]))
                          :branch-fn :childs
                          :childs-fn :childs
                          :id-fn :node-id
                          :v-gap 200})
        analysis-layout-size (layout-size analysis-layout)
        high-level-layout (layout-tree
                           (graph->nested-tree high-level-graph)
                           {:sizes {:read-ret          [200 80]
                                    :repl-wrapping-ret [200 80]
                                    :analysis-ret      [(:width analysis-layout-size) (:height analysis-layout-size)]
                                    :emission-ret      [200 80]
                                    :output            [200 80]}
                            :branch-fn :childs
                            :childs-fn :childs
                            :id-fn :node-id
                            :v-gap 100})
        high-level-layout-size (layout-size high-level-layout)
        analysis-box-layout (:analysis-ret high-level-layout)
        high-level-pane (build-high-level-pane high-level-graph high-level-layout)
        analysis-pane (build-analysis-pane analysis-graph
                                           (translate-layout [(:x analysis-box-layout)
                                                              (:y analysis-box-layout)]
                                                             analysis-layout))

        dia-pane (doto (ui/pane :childs [high-level-pane analysis-pane])
                   (.setPrefWidth (:width high-level-layout-size))
                   (.setPrefHeight (:height high-level-layout-size)))]
    dia-pane))


(fs-plugins/register-plugin
 :cljs-compiler
 {:label "Cljs compiler"
  :on-create (fn [_]
               (let [scroll-pane (ui/scroll-pane)
                     flow-id-txt (TextField. "0")
                     refresh-btn (doto (Button. "Refresh")
                                   (.setOnAction
                                    (reify javafx.event.EventHandler
                                      (handle [_ _]
                                        (let [compilation-graphs
                                              (runtime-api/call-by-fn-key rt-api
                                                                          :plugins.cljs-compiler/extract-compilation-graphs
                                                                          [(parse-long (.getText flow-id-txt))])

                                              diagram-pane (build-diagram-pane compilation-graphs)
                                              scale (Scale. 1 1 0 0)
                                              *last-coord (atom [0 0])]

                                          (-> diagram-pane .getTransforms (.add scale))

                                          (doto diagram-pane
                                            (.setOnScroll (ui-utils/event-handler
                                                              [sev]
                                                            (let [delta-y (.getDeltaY sev)
                                                                  curr-factor (.getX scale)
                                                                  new-factor (cond
                                                                               (pos? delta-y) (* curr-factor 1.1)
                                                                               (neg? delta-y) (/ curr-factor 1.1)
                                                                               :else          curr-factor)]
                                                              (.setX scale new-factor)
                                                              (.setY scale new-factor))
                                                            (.consume sev)))
                                            (.setOnMousePressed (ui-utils/event-handler
                                                                    [mev]
                                                                  (reset! *last-coord [(.getSceneX mev) (.getSceneY mev)])))
                                            (.setOnMouseDragged (ui-utils/event-handler
                                                                    [mev]
                                                                  (let [curr-factor (.getX scale)
                                                                        [last-x last-y] @*last-coord
                                                                        curr-x (.getSceneX mev)
                                                                        curr-y (.getSceneY mev)
                                                                        delta-x (- curr-x last-x)
                                                                        delta-y (- curr-y last-y)]
                                                                    (reset! *last-coord [curr-x curr-y])
                                                                    (.setHvalue scroll-pane (- (.getHvalue scroll-pane) (/ delta-x (.getWidth diagram-pane))))
                                                                    (.setVvalue scroll-pane (- (.getVvalue scroll-pane) (/ delta-y (.getHeight diagram-pane))))))))

                                          (.setContent scroll-pane diagram-pane))))))
                     tools-box (ui/h-box :childs [(Label. "FlowId:") flow-id-txt refresh-btn])
                     dw-pane (data-windows/data-window-pane {:data-window-id :plugins/cljs-compiler})
                     main-box (ui/border-pane
                               :top tools-box
                               :center (ui/split :orientation :horizontal
                                                 :sizes [0.7]
                                                 :childs [scroll-pane
                                                          dw-pane]))]
                 {:fx/node main-box}))})
