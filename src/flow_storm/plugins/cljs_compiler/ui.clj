(ns flow-storm.plugins.cljs-compiler.ui
  (:require [flow-storm.debugger.ui.plugins :as fs-plugins]
            [flow-storm.debugger.runtime-api :as runtime-api :refer [rt-api]]
            [flow-storm.debugger.ui.flows.screen :refer [goto-location]]
            [flow-storm.debugger.ui.components :as ui]
            [flow-storm.debugger.ui.utils :as ui-utils]
            [flow-storm.debugger.ui.data-windows.data-windows :as data-windows]
            [clj-tree-layout.core :refer [layout-tree]])
  (:import [javafx.scene.transform Scale Translate Rotate]
           [javafx.scene.shape Line Path MoveTo LineTo]
           [javafx.scene Node Group]))

(def read-ret-node-width 3500)
(def read-ret-node-height 800)

(def repl-wrapping-ret-node-width 3500)
(def repl-wrapping-ret-node-height 800)

(def analysis-nodes-width 400)
(def analysis-nodes-height 150)

(def emission-nodes-width 300)
(def emission-nodes-height 250)


(defn- graph->nested-tree
  ([{:keys [nodes] :as g}]
   (let [root (some (fn [node] (when (:root? node) node))
                    (vals nodes))]
     (graph->nested-tree g root)))

  ([{:keys [nodes edges] :as g} node]
   (let [childs (mapv (fn [nid] (graph->nested-tree g (get nodes nid))) (get edges (:node-id node)))]
     (assoc node :childs childs))))

(defn- labeled-container [& {:keys  [label label-scale x y width height on-ret-click on-goto-ret-click child]}]
  (let [ret-btn (ui/button :label "View return"
                           :on-click on-ret-click
                           :classes ["action-btn"])
        goto-ret-btn (ui/button :label "Goto return"
                                :classes ["action-btn"]
                                :on-click on-goto-ret-click)
        title-lbl (ui/label :text label
                      :class "title")

        top-box (ui/h-box
                 :childs [title-lbl ret-btn goto-ret-btn]
                 :spacing 10
                 :class "bottom-bar"
                 :align :center-left)]

    (-> top-box .getTransforms (.add label-scale))

    (doto (ui/border-pane
           :top    top-box
           :center child
           :class "labeled-container")
      (.setLayoutX x)
      (.setLayoutY y)
      (.setPrefWidth width)
      (.setPrefHeight height #_(+ height ret-btn-height 20)))))

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
               (.setOnMouseClicked
                (ui-utils/event-handler
                    [mev]
                  (when on-click
                    (on-click mev)))))
        head-path (Path.)
        trans (Translate. to-x to-y)
        line-angle (clalc-line-angle [from-x from-y] [to-x to-y])
        rot (Rotate. (+ line-angle 90))]
    (ui-utils/add-class line "arrow")
    (ui-utils/add-class head-path "arrow")
    (doto (.getElements head-path)
      (.add (MoveTo. 0 0))
      (.add (LineTo. -10 20))
      (.add (MoveTo. 0 0))
      (.add (LineTo. 10 20)))

    (-> head-path .getTransforms (.addAll [trans rot]))
    (Group. (into-array Node [head-path line]))))

(defn- build-parse-node [{:keys [form-prev]}]
  (ui/v-box
   :childs [(ui/label :text "cljs.analyzer/parse" :class "fn-name")
            (ui/label :text form-prev :class "form")]
   :spacing 10))

(defn- build-analysis-node [flow-id thread-id{:keys [form-prev passes]}]
  (let [{:keys [add-all list-view-pane]} (ui/list-view :editable? false
                                                       :cell-factory (fn [list-cell {:keys [pass-name]}]
                                                                       (-> list-cell
                                                                           (ui-utils/set-text nil)
                                                                           (ui-utils/set-graphic (ui/label :text pass-name))))
                                                       :on-click (fn [mev sel-items _]
                                                                   (when (ui-utils/double-click? mev)
                                                                     (let [pass (first sel-items)]
                                                                       (goto-location {:flow-id flow-id
                                                                                       :thread-id thread-id
                                                                                       :idx (:idx pass)}))))
                                                       :selection-mode :single)]
    (add-all passes)
    (ui/v-box
     :childs [(ui/h-box :childs [(ui/label :text "cljs.analyzer/analyze*" :class "fn-name")]
                        :spacing 10)
              (ui/label :text form-prev :class "form")
              (ui/label :text "Passes:")
              list-view-pane]
     :spacing 10)))

(defn- build-analysis-pane [flow-id thread-id {:keys [nodes edges]} node-id->layout]
  (let [nodes-vec (reduce-kv (fn [acc nid node]
                               (let [{:keys [x y width height]} (node-id->layout nid)
                                     view-args-btn (ui/button :label "View args"
                                                              :classes ["action-btn"]
                                                              :on-click (fn []
                                                                          (let [{:keys [fn-args-ref]} node]
                                                                            (runtime-api/data-window-push-val-data rt-api
                                                                                                                   :plugins/cljs-compiler
                                                                                                                   fn-args-ref
                                                                                                                   {:flow-storm.debugger.ui.data-windows.data-windows/dw-id :plugins/cljs-compiler
                                                                                                                    :flow-storm.debugger.ui.data-windows.data-windows/stack-key "FnCall"
                                                                                                                    :root? true}))))
                                     view-ret-btn (ui/button :label "View return"
                                                             :classes ["action-btn"]
                                                             :on-click (fn []
                                                                         (let [{:keys [ret-ref throwable-ref]} node]
                                                                           (runtime-api/data-window-push-val-data rt-api
                                                                                                                  :plugins/cljs-compiler
                                                                                                                  (or ret-ref throwable-ref)
                                                                                                                  {:flow-storm.debugger.ui.data-windows.data-windows/dw-id :plugins/cljs-compiler
                                                                                                                   :flow-storm.debugger.ui.data-windows.data-windows/stack-key "FnReturn"
                                                                                                                   :root? true}))))
                                     goto-call-btn (ui/button :label "Goto call"
                                                              :classes ["action-btn"]
                                                              :on-click (fn []
                                                                          (let [{:keys [idx]} node]
                                                                            (goto-location {:flow-id flow-id
                                                                                            :thread-id thread-id
                                                                                            :idx idx}))))
                                     goto-ret-btn (ui/button :label "Goto return"
                                                             :classes ["action-btn"]
                                                             :on-click (fn []
                                                                         (let [{:keys [idx]} node]
                                                                           (goto-location {:flow-id flow-id
                                                                                       :thread-id thread-id
                                                                                       :idx idx}))))
                                     node-box (doto
                                                  (ui/v-box
                                                   :childs [(case (:type node)
                                                              :analysis (build-analysis-node flow-id thread-id node)
                                                              :parsing  (build-parse-node node))
                                                            (ui/h-box :childs [view-args-btn goto-call-btn view-ret-btn goto-ret-btn]
                                                                      :spacing 10)]
                                                   :class (cond
                                                            (not (:read-form-analysis? node))
                                                            "non-read-form-node"

                                                            (= :analysis (:type node))
                                                            "analysis-node"

                                                            (= :parsing (:type node))
                                                            "parsing-node"))
                                                (.setLayoutX x)
                                                (.setLayoutY y)
                                                (.setPrefWidth width)
                                                (.setPrefHeight height)
                                                (.setMaxWidth analysis-nodes-width)
                                                (.setMaxHeight analysis-nodes-height))]

                                 (conj acc node-box)))
                             []
                             nodes)
        arrows-vec (reduce-kv (fn [arrows node-id-from to-ids]
                                (reduce (fn [arrs node-id-to]
                                          (let [node-from (node-id->layout node-id-from)
                                                node-to (node-id->layout node-id-to)
                                                from-x (+ (:x node-from) (/ (:width node-from) 2))
                                                from-y (+ (:y node-from) (:height node-from))
                                                to-x   (+ (:x node-to) (/ (:width node-to) 2))
                                                to-y   (:y node-to)

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

(defn- build-emission-node [flow-id thread-id {:keys [ast-op idx fn-args-ref write-outs read-form-emission?]}]
  (let [{:keys [add-all list-view-pane]} (ui/list-view :editable? false
                                                       :cell-factory (fn [list-cell {:keys [write-out]}]
                                                                       (-> list-cell
                                                                           (ui-utils/set-text nil)
                                                                           (ui-utils/set-graphic (ui/label :text write-out))))
                                                       :on-click (fn [mev sel-items _]
                                                                   (when (ui-utils/double-click? mev)
                                                                     (let [write-out (first sel-items)]
                                                                       (goto-location {:flow-id flow-id
                                                                                       :thread-id thread-id
                                                                                       :idx (:idx write-out)}))))
                                                       :selection-mode :single)

        view-args-btn (ui/button :label "View args"
                                 :classes ["action-btn"]
                                 :on-click (fn []
                                             (runtime-api/data-window-push-val-data rt-api
                                                                                    :plugins/cljs-compiler
                                                                                    fn-args-ref
                                                                                    {:flow-storm.debugger.ui.data-windows.data-windows/dw-id :plugins/cljs-compiler
                                                                                     :flow-storm.debugger.ui.data-windows.data-windows/stack-key "FnCall"
                                                                                     :root? true})))
        goto-call-btn (ui/button :label "Goto call"
                                 :classes ["action-btn"]
                                 :on-click (fn []
                                             (goto-location {:flow-id flow-id
                                                             :thread-id thread-id
                                                             :idx idx})))]
    (add-all write-outs)
    (ui/v-box
     :childs [(ui/label :text "cljs.compiler/emit" :class "fn-name")
              (ui/h-box :childs [view-args-btn goto-call-btn]
                        :spacing 10)
              (ui/label :text (str "Ast OP: " (pr-str ast-op)))
              (ui/label :text "Write outs:")
              list-view-pane]
     :spacing 10
     :class (if read-form-emission?
              "non-read-form-node"
              "emission-node"))))

(defn- build-emission-pane [flow-id thread-id {:keys [nodes edges]} node-id->layout]
  (let [nodes-vec (reduce-kv (fn [acc nid node]
                               (let [{:keys [x y width height]} (node-id->layout nid)
                                     node-box (case (:type node)
                                                :emission (build-emission-node flow-id thread-id node))
                                     node-box (doto node-box

                                                (.setLayoutX x)
                                                (.setLayoutY y)
                                                (.setPrefWidth width)
                                                (.setPrefHeight height)
                                                (.setMaxWidth emission-nodes-width)
                                                (.setMaxHeight emission-nodes-height))]

                                 (conj acc node-box)))
                             []
                             nodes)
        arrows-vec (reduce-kv (fn [arrows node-id-from to-ids]
                                (reduce (fn [arrs node-id-to]
                                          (let [node-from (node-id->layout node-id-from)
                                                node-to (node-id->layout node-id-to)
                                                from-x (+ (:x node-from) (/ (:width node-from) 2))
                                                from-y (+ (:y node-from) (:height node-from))
                                                to-x   (+ (:x node-to) (/ (:width node-to) 2))
                                                to-y   (:y node-to)

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

(defn- build-high-level-pane [flow-id thread-id
                              {:keys [nodes edges]} node-id->layout
                              {:keys [stages-labels-scale]} {:keys [analysis-pane emission-pane]}]
  (let [nodes-vec (reduce-kv (fn [acc nid node]
                               (let [{:keys [x y width height]} (node-id->layout nid)]
                                 (conj acc (labeled-container
                                            :label (case nid
                                                     :read-ret          "Reader"
                                                     :repl-wrapping-ret "Repl wrapping"
                                                     :analysis          "Analysis"
                                                     :emission          "Emission")
                                            :child (case nid
                                                     :read-ret          (ui/pane)
                                                     :repl-wrapping-ret (ui/pane)
                                                     :analysis          analysis-pane
                                                     :emission          emission-pane)
                                            :label-scale stages-labels-scale
                                            :x x
                                            :y y
                                            :width width
                                            :height height
                                            :on-ret-click (fn []
                                                            (let [{:keys [data node-id]} node
                                                                  val-ref (case node-id
                                                                            :read-ret          (:result data)
                                                                            :repl-wrapping-ret (:result data)
                                                                            :analysis          (-> data :fn-return :result)
                                                                            :emission          (-> data :fn-return :result))]
                                                              (runtime-api/data-window-push-val-data rt-api
                                                                                                     :plugins/cljs-compiler
                                                                                                     val-ref
                                                                                                     {:flow-storm.debugger.ui.data-windows.data-windows/dw-id :plugins/cljs-compiler
                                                                                                      :flow-storm.debugger.ui.data-windows.data-windows/stack-key "Data"
                                                                                                      :root? true})))
                                            :on-goto-ret-click (fn []
                                                                 (let [{:keys [data]} node]
                                                                   (let [idx (or (:idx data) (-> data :fn-return :idx))]
                                                                     (goto-location {:flow-id flow-id
                                                                                     :thread-id thread-id
                                                                                     :idx idx}))))))))
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

(defn build-diagram-pane [{:keys [flow-id thread-id high-level-graph analysis-graph emission-graph]} scales]
  (let [stage-padding 100
        analysis-layout (layout-tree
                         (graph->nested-tree analysis-graph)
                         {:sizes (zipmap (keys (:nodes analysis-graph)) (repeat [analysis-nodes-width analysis-nodes-height]))
                          :branch-fn :childs
                          :childs-fn :childs
                          :id-fn :node-id
                          :v-gap 200
                          :h-gap 30})
        analysis-layout-size (layout-size analysis-layout)

        emission-layout (layout-tree
                         (graph->nested-tree emission-graph)
                         {:sizes (zipmap (keys (:nodes emission-graph)) (repeat [emission-nodes-width emission-nodes-height]))
                          :branch-fn :childs
                          :childs-fn :childs
                          :id-fn :node-id
                          :v-gap 200
                          :h-gap 30})
        emission-layout-size (layout-size emission-layout)

        high-level-layout (layout-tree
                           (graph->nested-tree high-level-graph)
                           {:sizes {:read-ret          [read-ret-node-width read-ret-node-height]
                                    :repl-wrapping-ret [repl-wrapping-ret-node-width repl-wrapping-ret-node-height]
                                    :analysis          [(+ stage-padding (:width analysis-layout-size)) (+ stage-padding (:height analysis-layout-size))]
                                    :emission          [(+ stage-padding (:width emission-layout-size)) (+ stage-padding (:height emission-layout-size))]}
                            :branch-fn :childs
                            :childs-fn :childs
                            :id-fn :node-id
                            :v-gap 500})
        high-level-layout-size (layout-size high-level-layout)

        analysis-box-layout (:analysis high-level-layout)
        emission-box-layout (:emission high-level-layout)

        analysis-pane (doto (build-analysis-pane flow-id thread-id analysis-graph analysis-layout)
                        (.setLayoutX (:x analysis-box-layout))
                        (.setLayoutY (:y analysis-box-layout))
                        (.setPrefWidth (:width analysis-layout-size))
                        (.setPrefHeight (:height analysis-layout-size)))

        emission-pane (doto (build-emission-pane flow-id thread-id emission-graph emission-layout)
                        (.setLayoutX (:x emission-box-layout))
                        (.setLayoutY (:y emission-box-layout))
                        (.setPrefWidth (:width emission-layout-size))
                        (.setPrefHeight (:height emission-layout-size)))

        high-level-pane (doto (build-high-level-pane flow-id thread-id
                                                     high-level-graph
                                                     high-level-layout
                                                     scales
                                                     {:analysis-pane analysis-pane
                                                      :emission-pane emission-pane})
                          (.setPrefWidth (:width high-level-layout-size))
                          (.setPrefHeight (:height high-level-layout-size)))

        dia-pane (ui/pane :childs [high-level-pane])]
    dia-pane))

(defn clamp [x from to]
  (-> x
      (min to)
      (max from)))

(defn diagram-scale-interpolation [x]
  ;; some grpahtoy.com fun
  (clamp (* 0.5 (+ 1 (Math/sin (- (* x 3) 1.6))))
         0.05
         1))

(defn stages-labels-scale-interpolation [x]
  ;; some grpahtoy.com fun
  (clamp (* 7 (+ 1 (Math/sin (+ (* x 3) 1.8))))
         1
         14))

(defn- refresh-click [flow-id {:keys [on-new-diagram-pane]}]
  (let [compilation-graphs
        (runtime-api/call-by-fn-key rt-api
                                    :plugins.cljs-compiler/extract-compilation-graphs
                                    [flow-id
                                     {:exclude-repl-wrapping? true}])

        *zoom-perc (atom 0.2)
        init-dia-scale (diagram-scale-interpolation @*zoom-perc)
        init-labels-scale (stages-labels-scale-interpolation @*zoom-perc)
        diagram-scale (Scale. init-dia-scale init-dia-scale)
        stages-labels-scale (Scale. init-labels-scale init-labels-scale)
        diagram-pane (build-diagram-pane compilation-graphs
                                         {:diagram-scale diagram-scale
                                          :stages-labels-scale stages-labels-scale})
        translation (Translate. 0 0)

        *last-coord (atom [0 0])]

    (-> diagram-pane .getTransforms (.addAll [diagram-scale translation]))


    (doto diagram-pane
      (.setOnScroll (ui-utils/event-handler
                        [sev]
                      (let [delta-y (.getDeltaY sev)]
                        (swap! *zoom-perc (fn [zp]
                                            (-> (cond
                                                  (pos? delta-y) (+ zp 0.02)
                                                  (neg? delta-y) (- zp 0.02)
                                                  :else zp)
                                                (min 1)
                                                (max 0))))
                        (let [zp @*zoom-perc
                              new-dia-scale (diagram-scale-interpolation zp)
                              new-stages-labels-scale (stages-labels-scale-interpolation zp)]
                          (.setX diagram-scale new-dia-scale)
                          (.setY diagram-scale new-dia-scale)
                          (.setX stages-labels-scale new-stages-labels-scale)
                          (.setY stages-labels-scale new-stages-labels-scale)))
                      (.consume sev)))
      (.setOnMousePressed (ui-utils/event-handler
                              [mev]
                            (reset! *last-coord [(.getSceneX mev) (.getSceneY mev)])))
      (.setOnMouseDragged (ui-utils/event-handler
                              [mev]
                            (let [curr-scale (.getX diagram-scale)
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

(defn- build-toolbar [flow-cmb  {:keys [on-reload-click]}]
  (let [reload-btn (ui/icon-button
                    :icon-name "mdi-reload"
                    :on-click on-reload-click
                    :tooltip "Reload compiler graph")]
    (ui/v-box :childs [(ui/h-box :childs [(ui/label :text "Flow-id :") flow-cmb reload-btn]
                                 :spacing 5)]
              :spacing 5)))

(defn- on-create [_]
  (let [digram-outer-pane (ui/pane :childs [])
        *flow-id (atom 0)
        flow-cmb (ui/combo-box :items []
                               :cell-factory (fn [_ flow-id] (ui/label :text (str flow-id)))
                               :button-factory (fn [_ flow-id] (ui/label :text (str flow-id)))
                               :on-change (fn [_ flow-id] (when flow-id (reset! *flow-id flow-id))))
        set-diagram-pane (fn [p]
                           (.clear (.getChildren digram-outer-pane))
                           (.add (.getChildren digram-outer-pane) p))
        toolbar-pane (build-toolbar flow-cmb
                                    {:on-reload-click
                                     (fn []
                                       (let [flow-id @*flow-id]
                                         (refresh-click flow-id {:on-new-diagram-pane set-diagram-pane})))})

        dw-pane (data-windows/data-window-pane {:data-window-id :plugins/cljs-compiler})
        main-box (ui/border-pane
                  :top toolbar-pane
                  :center (ui/split :orientation :horizontal
                                    :sizes [0.7]
                                    :childs [digram-outer-pane dw-pane]))]
    {:fx/node main-box
     :flow-cmb flow-cmb
     :selected-flow-id-ref *flow-id
     :flow-clear (fn [flow-id])}))

(defn- on-focus [{:keys [flow-cmb selected-flow-id-ref]}]
  (let [flow-ids (into #{} (map first (runtime-api/all-flows-threads rt-api)))]
    (ui-utils/combo-box-set-items flow-cmb flow-ids)
    (ui-utils/combo-box-set-selected flow-cmb @selected-flow-id-ref)))

(defn- on-flow-clear [flow-id {:keys [flow-clear]}]
  (flow-clear flow-id))

(fs-plugins/register-plugin
 :cljs-compiler
 {:label "Cljs compiler"
  :css-resource       "flow-storm-cljs-compiler-plugin/styles.css"
  :dark-css-resource  "flow-storm-cljs-compiler-plugin/dark.css"
  :light-css-resource "flow-storm-cljs-compiler-plugin/light.css"
  :on-focus on-focus
  :on-create on-create
  :on-flow-clear on-flow-clear})
