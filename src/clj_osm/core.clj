(ns clj-osm.core
  (:use [clojure.data.zip.xml :only (attr text xml->)])
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io]))

(defn- tags->map [nodes]
  (->> nodes
       (filter #(= :tag (:tag %)))
       (map (fn [{:keys [attrs]}] [(:k attrs) (:v attrs)]))
       (into {})))

(defmulti parse-xml-node :tag)

(defmethod parse-xml-node :node [node]
  {:type :node
   :id (get-in node [:attrs :id])
   :lon (BigDecimal. (get-in node [:attrs :lon]))
   :lat (BigDecimal. (get-in node [:attrs :lat]))})

(defmethod parse-xml-node :way [node]
  {:type :way
   :id (get-in node [:attrs :id])
   :nodes (->> (:content node)
               (filter #(= :nd (:tag %)))
               (map #(get-in % [:attrs :ref]) ))
   :tags (tags->map (:content node))})

(defmethod parse-xml-node :relation [node]
  {:type :relation
   :id (get-in node [:attrs :id])
   :members (->> (:content node)
                 (filter #(= :member (:tag %)))
                 (map :attrs)
                 (map #(select-keys % [:type :ref :role]) ))
   :tags (tags->map (:content node))})

(defmethod parse-xml-node :bounds [node]
  ;;  <bounds minlat="51.5131300" minlon="-0.1007000" maxlat="51.5146100" maxlon="-0.0964900"/>
  {:type :bounds
   :min-lat (get-in node [:attrs :minlat])
   :min-lon (get-in node [:attrs :minlon])
   :max-lat (get-in node [:attrs :maxlat])
   :max-lon (get-in node [:attrs :maxlon])})

(defn into-map [v]
  (->> v
       (map (fn [e] [(:id e) e]))
       (into {})))

(defn parse-xml-nodes [zipped]
  (->> zipped
       first
       :content
       (map parse-xml-node)
       (group-by :type)))

(defn- lookup-nodes [nodes way]
  (update way :nodes #(map (partial get nodes) %)))

(defn osm-file->waypoints [filename]
  (let [{:keys [node way]} (-> filename xml/parse zip/xml-zip parse-xml-nodes)
        node-map (into-map node)]
    (map (partial lookup-nodes node-map) way)))

(defn osm-file->data [filename]
  (-> filename xml/parse zip/xml-zip parse-xml-nodes))
