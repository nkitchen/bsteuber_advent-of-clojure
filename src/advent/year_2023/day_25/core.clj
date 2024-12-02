(ns advent.year-2023.day-25.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapcat (fn [line]
                 (let [[left-module rhs] (str/split line #": ")
                       right-modules (str/split rhs #" ")]
                   (mapcat (fn [right-module]
                             [[left-module right-module]
                              [right-module left-module]])
                           right-modules))))
       (reduce (fn [graph [left right]]
                 (update graph left (fnil conj #{}) right))
               {})))

(defn select-random-edge [multigraph])

(defn merge-nodes [{:keys [nodes edges]} node-1 node-2]
  (let [all-edges (->> (concat (get nodes node-1)
                               (get nodes node-2))
                       (remove #{#{node-1 node-2}}))
        new-node (vec (concat node-1 node-2))]
    (reduce (fn [{:keys [nodes edges]} edge]
              (let [new-edge (->> edge
                                  )])
              {:nodes (update nodes new-node (fnil conj #{}) edge)
               :edges })
            {:nodes (dissoc nodes node-1 node-2)
             :edges edges}
            all-edges)))

(defn try-solving [multigraph max-cuts]
  (loop [multigraph multigraph]
    (let [[first-edge & more-edges] multigraph]
      (if (empty? more-edges)
        (when (<= (second first-edge)
                  max-cuts)
          (let [[merged-nodes-1 merged-nodes-2] (first first-edge)]
            (* merged-nodes-1 merged-nodes-2)))
        (let [[node-1 node-2] (select-random-edge multigraph)]
          (recur (merge-nodes multigraph node-1 node-2)))))))

(defn part-1 [file]
  (let [multigraph (read-input file)]
    (->> (range 10000)
         (some (fn [i]
                 (when-let [result (try-solving multigraph 3)]
                   [i result]))))))

(comment
  (read-input "test"))
