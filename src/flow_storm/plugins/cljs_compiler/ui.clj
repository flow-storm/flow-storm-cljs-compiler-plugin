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
           [javafx.scene.transform Scale Translate Rotate]
           [javafx.scene.layout HBox VBox]
           [javafx.scene.shape Line Rectangle Path MoveTo LineTo]
           [javafx.event EventHandler]
           [javafx.scene Node Group]))

(defn- graph->nested-tree
  ([{:keys [nodes edges] :as g}]
   (let [root (some (fn [node] (when (:root? node) node))
                    (vals nodes))]
     (graph->nested-tree g root)))

  ([{:keys [nodes edges] :as g} node]
   (let [childs (mapv (fn [nid] (graph->nested-tree g (get nodes nid))) (get edges (:node-id node)))]
     (assoc node :childs childs))))

(defn- labeled-container [& {:keys  [label x y width height on-ret-click]}]
  (let [ret-btn (ui/button :label "Return"
                           :on-click on-ret-click)
        lbl (doto (ui/label :text label)
              (.setLayoutX 10)
              (.setLayoutY 10))
        ret-btn-width 100
        ret-btn-height 50]
    (doto ret-btn
      (.setLayoutX (+ (/ width 2) (- (/ ret-btn-width 2))))
      (.setLayoutY height)
      (.setPrefWidth 100)
      (.setPrefHeight 50)
      (.toFront))
    (doto (ui/pane
           :childs
           [lbl ret-btn])
      (.setStyle "-fx-background-color: #AAA")
      (.setLayoutX x)
      (.setLayoutY y)
      (.setPrefWidth width)
      (.setPrefHeight (+ height ret-btn-height 20)))))

(defn- labeled-node [& {:keys  [label x y width height]}]
  (doto (ui/label :text label)
    (.setLayoutX x)
    (.setLayoutY y)
    (.setPrefWidth width)
    (.setPrefHeight height)
    (.setStyle "-fx-border-color:#333; -fx-border-width: 5; -fx-background-color: pink;")))

(defn clalc-line-angle
  "Calculates the angle of the line passing through points (x1, y1) and (x2, y2).
   The angle is returned in degrees, measured counterclockwise from the positive x-axis."
  [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        radians (Math/atan2 dy dx)
        degrees (Math/toDegrees radians)]
    (if (neg? degrees)
      (+ degrees 360) ; Ensure the angle is in [0, 360) range
      degrees)))

(defn- arrow [& {:keys [from-x from-y to-x to-y on-click]}]
  (let [line (doto (Line. from-x from-y to-x to-y)
               (.toFront)
               (.setPickOnBounds true)
               (.setStyle "-fx-stroke-width: 5;")
               (.setOnMouseClicked
                (ui-utils/event-handler
                    [mev]
                  (on-click mev))))
        head-path (doto (Path.)
                    (.setStyle "-fx-stroke-width: 5;"))
        trans (Translate. to-x to-y)
        line-angle (clalc-line-angle [from-x from-y] [to-x to-y])
        rot (Rotate. (+ line-angle 90))]
    (doto (.getElements head-path)
      (.add (MoveTo. 0 0))
      (.add (LineTo. -10 20))
      (.add (MoveTo. 0 0))
      (.add (LineTo. 10 20)))

    (-> head-path .getTransforms (.addAll [trans rot]))
    (Group. (into-array Node [head-path line]))))

(defn- build-analysis-pane [{:keys [nodes edges]} node-id->layout]
  (let [nodes-vec (reduce-kv (fn [acc nid node]
                               (let [{:keys [x y width height]} (node-id->layout nid)
                                     node (nodes nid)
                                     fn-lbl (case (:type node)
                                              :analysis "cljs.analyzer/analyze*"
                                              :parsing  "cljs.analyzer/parse")
                                     call-btn (ui/button :label "Call"
                                                         :on-click (fn []
                                                                     (let [{:keys [fn-args-ref]} node]
                                                                       (runtime-api/data-window-push-val-data rt-api
                                                                                                              :plugins/cljs-compiler
                                                                                                              fn-args-ref
                                                                                                              {:flow-storm.debugger.ui.data-windows.data-windows/dw-id :plugins.cljs-compiler/extract-compilation-tree
                                                                                                               :flow-storm.debugger.ui.data-windows.data-windows/stack-key "FnCall"
                                                                                                               :root? true}))))
                                     ret-btn (ui/button :label "Return"
                                                        :on-click (fn []
                                                                    (let [{:keys [ret-ref throwable-ref]} node]
                                                                      (runtime-api/data-window-push-val-data rt-api
                                                                                                             :plugins/cljs-compiler
                                                                                                             (or ret-ref throwable-ref)
                                                                                                             {:flow-storm.debugger.ui.data-windows.data-windows/dw-id :plugins.cljs-compiler/extract-compilation-tree
                                                                                                              :flow-storm.debugger.ui.data-windows.data-windows/stack-key "FnReturn"
                                                                                                              :root? true}))))
                                     node-box (doto
                                                  (ui/v-box
                                                   :childs [(ui/h-box :childs [(ui/label :text fn-lbl) call-btn ret-btn]
                                                                      :spacing 10)
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
                                                from-x (+ (:x node-from) (/ (:width node-from) 2))
                                                from-y (+ (:y node-from) (:height node-from))
                                                to-x   (+ (:x node-to) (/ (:width node-to) 2))
                                                to-y   (:y node-to)

                                                node (nodes node-id-from)
                                                arr (arrow :from-x from-x
                                                           :from-y from-y
                                                           :to-x   to-x
                                                           :to-y   to-y)]
                                            (into arrs [arr])))
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
                                                              :height height
                                                              :on-ret-click (fn []
                                                                              (let [{:keys [entry]} node
                                                                                    val-ref (case (:type entry)
                                                                                              :fn-return (:result entry)
                                                                                              :expr (:result entry))]
                                                                                (runtime-api/data-window-push-val-data rt-api
                                                                                                                       :plugins/cljs-compiler
                                                                                                                       val-ref
                                                                                                                       {:flow-storm.debugger.ui.data-windows.data-windows/dw-id :plugins.cljs-compiler/extract-compilation-tree
                                                                                                                        :flow-storm.debugger.ui.data-windows.data-windows/stack-key "Data"
                                                                                                                        :root? true})))))))
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
                                                arr (arrow :from-x from-x
                                                           :from-y from-y
                                                           :to-x   to-x
                                                           :to-y   to-y)]
                                            (conj arrs arr)))
                                 arrows
                                 to-ids))
                          []
                          edges)
        nodes-and-arrows-vec (->> nodes-vec
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
                         {:sizes (zipmap (keys (:nodes analysis-graph)) (repeat [300 100]))
                          :branch-fn :childs
                          :childs-fn :childs
                          :id-fn :node-id
                          :v-gap 200
                          :h-gap 30})
        analysis-layout-size (layout-size analysis-layout)
        high-level-layout (layout-tree
                           (graph->nested-tree high-level-graph)
                           {:sizes {:read-ret          [200 80]
                                    :repl-wrapping-ret [200 80]
                                    :analysis-ret      [(:width analysis-layout-size) (:height analysis-layout-size)]
                                    :emission-ret      [200 80]}
                            :branch-fn :childs
                            :childs-fn :childs
                            :id-fn :node-id
                            :v-gap 200})
        high-level-layout-size (layout-size high-level-layout)
        analysis-box-layout (:analysis-ret high-level-layout)
        high-level-pane (doto (build-high-level-pane high-level-graph high-level-layout)
                          (.setPrefWidth (:width high-level-layout-size))
                          (.setPrefHeight (:height high-level-layout-size)))
        analysis-pane (doto (build-analysis-pane analysis-graph analysis-layout)
                        (.setLayoutX (:x analysis-box-layout))
                        (.setLayoutY (:y analysis-box-layout))
                        (.setPrefWidth (:width analysis-layout-size))
                        (.setPrefHeight (:height analysis-layout-size)))

        dia-pane (ui/pane :childs [high-level-pane analysis-pane])]
    dia-pane))

(defn- refresh-click [flow-id {:keys [on-new-diagram-pane]}]
  (let [compilation-graphs
        (runtime-api/call-by-fn-key rt-api
                                    :plugins.cljs-compiler/extract-compilation-graphs
                                    [flow-id
                                     {:exclude-repl-wrapping? true}])

        diagram-pane (build-diagram-pane compilation-graphs)
        translation (Translate. 0 0)
        scale (Scale. 1 1)
        *last-coord (atom [0 0])]

    (-> diagram-pane .getTransforms (.addAll [scale translation]))


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
                            (let [curr-scale (.getX scale)
                                  curr-trans-x (.getX translation)
                                  curr-trans-y (.getY translation)
                                  [last-x last-y] @*last-coord
                                  curr-x (.getSceneX mev)
                                  curr-y (.getSceneY mev)
                                  delta-x (/ (- curr-x last-x) curr-scale)
                                  delta-y (/ (- curr-y last-y) curr-scale)]
                              (reset! *last-coord [curr-x curr-y])
                              (.setX translation (+ curr-trans-x delta-x))
                              (.setY translation (+ curr-trans-y delta-y))))))
    (on-new-diagram-pane diagram-pane)))

(fs-plugins/register-plugin
 :cljs-compiler
 {:label "Cljs compiler"
  :on-create (fn [_]
               (let [digram-outer-pane (ui/pane :childs [])
                     set-diagram-pane (fn [p]
                                        (.clear (.getChildren digram-outer-pane))
                                        (.add (.getChildren digram-outer-pane) p))
                     flow-id-txt (ui/text-field :initial-text "0")
                     refresh-btn (ui/button :label "Refresh"
                                            :on-click (fn []
                                                        (let [flow-id (parse-long (.getText flow-id-txt))]
                                                          (refresh-click flow-id {:on-new-diagram-pane set-diagram-pane}))))
                     tools-box (ui/h-box :childs [(ui/label :text "FlowId:") flow-id-txt refresh-btn])
                     dw-pane (data-windows/data-window-pane {:data-window-id :plugins/cljs-compiler})
                     main-box (ui/border-pane
                               :top tools-box
                               :center (ui/split :orientation :horizontal
                                                 :sizes [0.7]
                                                 :childs [digram-outer-pane dw-pane]))]
                 {:fx/node main-box}))})
