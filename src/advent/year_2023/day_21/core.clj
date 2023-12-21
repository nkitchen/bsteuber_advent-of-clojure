(ns advent.year-2023.day-21.core
  (:require [advent.tools :as tools]
            [advent.grid :as grid]
            [clojure.set :as set]))

(defn read-input [file]
  (->> file
       tools/read-grid
       (reduce (fn [data [point ch]]
                 (cond-> data
                   (#{\. \S} ch)
                   (update :grid conj point)

                   (= ch \S)
                   (assoc :start point)))
               {:grid #{}})))

(defn solve [{:keys [grid start]} steps]
  (loop [steps (inc steps)
         seen #{}
         explore #{start}
         prev-fields 0
         prev-prev-fields 0]
    (if (zero? steps)
      prev-fields
      (let [seen (set/union seen explore)
            fields (+ prev-prev-fields (count explore))
            explore (->> explore
                         (mapcat grid/neighbors-4)
                         (filter grid)
                         (remove seen)
                         (into #{}))]
        (recur (dec steps)
               seen
               explore
               fields
               prev-fields)))))

(defn part-1 [file steps]
  (solve (read-input file) steps))

(comment
  (part-1 "test" 6)
  (part-1 "input" 64))