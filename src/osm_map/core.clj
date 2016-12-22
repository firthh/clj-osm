(ns osm-map.core
  (:use [clojure.data.zip.xml :only (attr text xml->)])
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io]))

(defn- get-node-type [node-type zipped]
  (->> zipped
       first
       :content
       (filter #(= node-type (:tag %)))))

(defn- tags->map [nodes]
  (->> nodes
       (filter #(= :tag (:tag %)))
       (map (fn [{:keys [attrs]}] [(:k attrs) (:v attrs)]))
       (into {})))

(defn- transform-xml-node [node]
  {:id (get-in node [:attrs :id])
   :lon (BigDecimal. (get-in node [:attrs :lon]))
   :lat (BigDecimal. (get-in node [:attrs :lat]))})

(defn- transform-xml-way [node]
  {:id (get-in node [:attrs :id])
   :nodes (->> (:content node)
               (filter #(= :nd (:tag %)))
               (map #(get-in % [:attrs :ref]) ))
   :tags (tags->map (:content node))})

(defn- get-nodes [zipped]
  (->> zipped
       (get-node-type :node)
       (map transform-xml-node)
       (map (fn [e] [(:id e) e]))
       (into {})))

(defn- get-waypoints [zipped]
  (->> zipped
       (get-node-type :way)
       (map transform-xml-way)))

(defn- lookup-nodes [nodes way]
  (update way :nodes #(map (partial get nodes) %)))



(defn osm-file->all-data [filename]
  (let [zipped (-> filename xml/parse zip/xml-zip)]
    {:nodes (get-nodes zipped)
     :ways (get-waypoints zipped)}))

(defn osm-file->waypoints [filename]
  (let [zipped (-> filename xml/parse zip/xml-zip)
        nodes (get-nodes zipped)]
    (->> (get-waypoints zipped)
         (map (partial lookup-nodes nodes)))))
