(ns advent.year-2024.day-23.core
  (:require
   [advent.tools :as tools]
   [clojure.string :as str]
   [clojure.set :as set]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (reduce (fn [edges line]
                 (let [[from to] (str/split line #"-")]
                   (-> edges
                       (update from (fnil conj #{}) to)
                       (update to (fnil conj #{}) from))))
               {})))

(defn part-1 [file]
  (let [edges (read-input file)]
    (->> edges
         (mapcat (fn [[node neighbors]]
                   (when (str/starts-with? node "t")
                     (->> neighbors
                          tools/all-pairs
                          (map (fn [[neighb-1 neighb-2]]
                                 (when ((edges neighb-1) neighb-2)
                                   #{node neighb-1 neighb-2})))
                          (filter some?)))))
         distinct
         count)))

(defn part-2 [file]
  (let [edges (read-input file)
        nodes (sort (keys edges))]
    (loop [subgraphs (->> nodes
                          (map sorted-set))]
      (let [next-subgraphs (->> subgraphs
                                (mapcat (fn [subgraph]
                                          (let [largest-node (last subgraph)]
                                            (->> nodes
                                                 (filter #(pos? (compare % largest-node)))
                                                 (filter (fn [node]
                                                           (empty?
                                                            (set/difference subgraph
                                                                            (edges node)))))
                                                 (map #(conj subgraph %)))))))]
        (if (empty? next-subgraphs)
          (str/join "," (first subgraphs))
          (recur next-subgraphs))))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
