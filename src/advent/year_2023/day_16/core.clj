(ns advent.year-2023.day-16.core
  (:require [advent.tools :as tools]
            [advent.grid :as grid]))

(defn next-beam-states [grid [point direction]]
  (let [ch          (grid point)
        horizontal? (#{grid/west grid/east} direction)
        directions  (case ch
                      \. [direction]
                      \/ [({grid/west grid/south
                            grid/east grid/north
                            grid/north grid/east
                            grid/south grid/west} direction)]
                      \\ [({grid/west grid/north
                            grid/east grid/south
                            grid/north grid/west
                            grid/south grid/east} direction)]
                      \- (if horizontal?
                           [direction]
                           [grid/west grid/east])
                      \| (if horizontal?
                           [grid/north grid/south]
                           [direction]))]
    (->> directions
         (map (fn [dir]
                (let [point (grid/go-dir point dir)]
                  (when (grid point)
                    [point dir]))))
         (filter some?))))

(defn solve [grid start]
  (loop [seen {}
         [explore-first & explore-more] [start]]
    (cond
      (nil? explore-first)
      (count seen)

      (get-in seen explore-first)
      (recur seen explore-more)

      :else
      (recur (assoc-in seen explore-first true)
             (concat explore-more (next-beam-states grid explore-first))))))

(defn part-1 [file]
  (let [grid (tools/read-grid file)
        start [[0 0] grid/east]]
    (solve grid start)))

(defn part-2 [file]
  (let [grid (tools/read-grid file)
        {:keys [rows cols]} (tools/read-grid-dimensions file)]
    (->> (concat
          (for [x (range cols)]
            [[x 0] grid/south])
          (for [x (range cols)]
            [[x (dec rows)] grid/north])
          (for [y (range rows)]
            [[0 y] grid/east])
          (for [y (range rows)]
            [[(dec cols) y] grid/west]))
         (map (partial solve grid))
         (apply max))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
