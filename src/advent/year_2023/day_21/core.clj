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

(defn part-1 [file steps]
  (let [{:keys [grid start]} (read-input file)]
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
                 prev-fields))))))

(defn calc-border-profile [grid start end]
  (loop [steps 0
         seen #{}
         explore (into #{} start)
         results {}
         todo (into #{} end)
         min-steps nil]
    (if (empty? todo)
      [min-steps results]
      (let [at-end  (set/intersection todo explore)
            min-steps (or min-steps
                          (when-not (empty? at-end)
                            steps))
            results (reduce (fn [results point]
                              (if (results point)
                                results
                                (assoc results point (- steps min-steps))))
                            results
                            at-end)
            seen (set/union seen explore)
            explore (->> explore
                         (mapcat grid/neighbors-4)
                         (filter grid)
                         (remove seen)
                         (into #{}))]
        (recur (inc steps)
               seen
               explore
               results
               (set/difference todo at-end)
               min-steps)))))

(defn grid-border [{:keys [rows cols]} dir]
  (let [[x-range y-range]
        (condp = dir
          grid/west [[0] (range rows)]
          grid/east [[(dec cols)] (range rows)]
          grid/north [(range cols) [0]]
          grid/south [(range cols) [(dec rows)]])]
    (for [x x-range
          y y-range]
      [x y])))

(defn part-2 [file]
  (let [{:keys [grid start]} (read-input file)
        dims (tools/read-grid-dimensions file)]
    (calc-border-profile grid [start] (grid-border dims grid/east))))

(comment
  (part-1 "test" 6)
  (part-1 "input" 64)
  (part-2 "test"))
