(ns advent.year-2024.day-12.core
  (:require
   [advent.tools :as tools]
   [advent.grid :as grid]))

(defn calc-region [grid point]
  (let [region-letter (grid point)
        same-region? (fn [pt]
                       (= region-letter
                          (grid pt)))]
    (loop [todo [point]
           points #{}
           edges #{}]
      (let [[current-point & remaining] todo]
        (cond
          (nil? current-point)
          [points edges]

          (points current-point)
          (recur (next todo) points edges)

          :else
          (let [[neighbours
                 new-edges] (->> grid/directions-4
                                 (reduce
                                  (fn [[neighbours edges] dir]
                                    (let [neigb (grid/go-dir current-point dir)]
                                      (if (same-region? neigb)
                                        [(conj neighbours neigb)
                                         edges]
                                        [neighbours
                                         (conj edges [dir current-point])])))
                                  [[] []]))]
            (recur (concat remaining neighbours)
                   (conj points current-point)
                   (into edges new-edges))))))))

(defn solve [edges->factor file]
  (let [grid (tools/read-grid file)]
    (->> (keys grid)
         (reduce (fn [[visited sum] point]
                   (if (visited point)
                     [visited sum]
                     (let [[points edges] (calc-region grid point)]
                       [(into visited points)
                        (+ sum (* (count points) (edges->factor edges)))])))
                 [#{} 0])
         second)))

(def part-1 (partial solve count))

(defn count-sides [edges]
  (->> edges
       (group-by first)
       (map (fn [[_ edges]]
              (->> edges
                   (map second)
                   sort
                   (reduce (fn [sides point]
                             (let [neighbs (->> grid/directions-4
                                                (map (partial grid/go-dir point)))
                                   index (->> sides
                                              (map-indexed (fn [i points]
                                                             (when (some points neighbs)
                                                               i)))
                                              (some identity))]
                               (if index
                                 (update sides index conj point)
                                 (conj sides #{point}))))
                           [])
                   count)))
       (apply +)))

(def part-2 (partial solve count-sides))

(comment
  (part-1 "test1")
  (part-1 "test2")
  (part-1 "test3")
  (part-1 "input")
  (part-2 "test1")
  (part-2 "test2")
  (part-2 "test3")
  (part-2 "input"))
