(ns flow-storm.plugins.cljs-compiler.ui
  (:require [flow-storm.debugger.ui.plugins :as fs-plugins]
            [flow-storm.debugger.runtime-api :as runtime-api :refer [rt-api]]
            [flow-storm.debugger.ui.components :as ui]
            [flow-storm.debugger.ui.utils :as ui-utils]
            [flow-storm.debugger.ui.data-windows.data-windows :as data-windows]
            [clojure.string :as str]
            [clj-tree-layout.core :refer [layout-tree]])
  (:import [javafx.scene.control Label Button TextField]
           [javafx.scene.layout HBox VBox]
           [javafx.scene.shape Line]
           [javafx.event EventHandler]
           [javafx.scene Node]))

(defn nodes-by-id [tree]
  (reduce (fn [nodes-by-id n]
            (assoc nodes-by-id (:node-id n) (dissoc n :childs)))
   {}
   (tree-seq :childs :childs tree)))

(defn edges [tree]
  (reduce (fn [edges {:keys [node-id childs]}]
            (into edges (mapv (fn [c] [node-id (:node-id c)]) childs)))
          []
          (tree-seq :childs :childs tree)))

(defn build-diagram-pane [outer-tree]
  (let [node-id->node (nodes-by-id outer-tree)
        node-id->layout (layout-tree
                         outer-tree
                         {:sizes (zipmap (mapv :node-id (tree-seq :childs :childs outer-tree))
                                         (repeat [200 80]))
                          :branch-fn :childs
                          :childs-fn :childs
                          :id-fn :node-id
                          :v-gap 100})
        nodes-vec (reduce-kv (fn [acc nid node]
                               (let [{:keys [x y width height]} (node-id->layout nid)]
                                 (conj acc
                                       (doto (ui/label :text (name nid))
                                         (.setLayoutX x)
                                         (.setLayoutY y)
                                         (.setPrefWidth width)
                                         (.setPrefHeight height)
                                         (.setStyle "-fx-border-color:#333; -fx-border-width: 5; -fx-background-color: pink;")))))
                             []
                             node-id->node)
        lines-vec (reduce (fn [lines [node-id-from node-id-to]]
                            (let [layout-from (node-id->layout node-id-from)
                                  layout-to (node-id->layout node-id-to)
                                  from-x (+ (:x layout-from) (/ (:width layout-from) 2))
                                  from-y (+ (:y layout-from) (:height layout-from))
                                  to-x   (+ (:x layout-to) (/ (:width layout-to) 2))
                                  to-y   (:y layout-to)
                                  {:keys [entry]} (node-id->node node-id-from)
                                  line (doto (Line. from-x from-y to-x to-y)
                                         (.setOnMouseClicked
                                          (ui-utils/event-handler
                                              [mev]
                                            (prn "@@@@ clicked" entry)
                                            (let [val-ref (case (:type entry)
                                                            :fn-return (:result entry)
                                                            :expr (:result entry))]
                                              (runtime-api/data-window-push-val-data rt-api
                                                                                     :plugins/cljs-compiler
                                                                                     val-ref
                                                                                     {:flow-storm.debugger.ui.data-windows.data-windows/dw-id :plugins.cljs-compiler/extract-compilation-tree
                                                                                      :flow-storm.debugger.ui.data-windows.data-windows/stack-key "Data"
                                                                                      :root? true}))
                                            )))]
                              (conj lines line)))
                   []
                   (edges outer-tree))
        nodes-and-lines-vec (-> nodes-vec
                                (into lines-vec))]
    (ui/pane :childs nodes-and-lines-vec)))


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
                                        (let [{:keys [outer-tree]} (runtime-api/call-by-fn-key rt-api
                                                                                               :plugins.cljs-compiler/extract-compilation-tree
                                                                                               [(parse-long (.getText flow-id-txt))])

                                              diagram-pane (build-diagram-pane outer-tree)]

                                          (-> diagram-pane
                                              .prefWidthProperty
                                              (.bind (.widthProperty scroll-pane)))

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
