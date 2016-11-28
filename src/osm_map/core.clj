(ns osm-map.core
  (:use [clojure.data.zip.xml :only (attr text xml->)])
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [quil.core :as q])
  (:gen-class))

(def xml (xml/parse "resources/map2.xml"))
(def zipped (zip/xml-zip xml))

(defn get-node-type [node-type zipped]
  (->> zipped
       first
       :content
       (filter #(= node-type (:tag %)))))

(defn tags->map [nodes]
  (->> nodes
       (filter #(= :tag (:tag %)))
       (map (fn [{:keys [attrs]}] [(:k attrs) (:v attrs)]))
       (into {})))

(defn transform-xml-node [node]
  {:id (get-in node [:attrs :id])
   :lon (BigDecimal. (get-in node [:attrs :lon]))
   :lat (BigDecimal. (get-in node [:attrs :lat]))})

(defn transform-xml-way [node]
  {:id (get-in node [:attrs :id])
   :nodes (->> (:content node)
               (filter #(= :nd (:tag %)))
               (map #(get-in % [:attrs :ref]) ))
   :tags (tags->map (:content node))})

(defn get-nodes [zipped]
  (->> zipped
       (get-node-type :node)
       (map transform-xml-node)
       (map (fn [e] [(:id e) e]))
       (into {})))

(defn get-waypoints [zipped]
  (->> zipped
       (get-node-type :way)
       (map transform-xml-way)))

(defn lookup-nodes [nodes way]
  (update way :nodes #(map (partial get nodes) %)))

(defn get-footways [zipped]
  (let [nodes (get-nodes zipped)]
    (->> (get-waypoints zipped)
         (filter #(not (get-in % [:tags "railway"])))
         (map (partial lookup-nodes nodes)))))

;; -----------------------------------------------------------------------
;; Some Quil related stuff

(defn abs [n] (max n (- n)))

(defn translate-lon [lon]
  (abs (+ (* 100000 lon) 014720
          )))

(defn translate-lat [lat]
  (abs (- (* 100000 lat) 5153480
          )))

(defn draw-line [p1 p2]
  (println (translate-lon (:lon p1)))
  (q/line (translate-lat (:lat p1))
          (translate-lon (:lon p1))
          (translate-lat (:lat p2))
          (translate-lon (:lon p2))))

(defn draw []
  (dorun
   (for [footway (get-footways zipped)
         [p1 p2] (partition 2 1 (:nodes footway))]
     (draw-line p1 p2))))

(defn create-sketch []
  (q/defsketch example                  ;; Define a new sketch named example
    :title "Oh so many grey circles"    ;; Set the title of the sketch
    :settings #(q/smooth 2)             ;; Turn on anti-aliasin
    :draw draw                          ;; Specify the draw fn
    :refresh-rate 1
    :size [1000 1000])                    ;; You struggle to beat the golden ratio
  )

(defn -main []
  (create-sketch))
