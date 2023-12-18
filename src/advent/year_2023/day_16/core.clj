(ns advent.year-2023.day-16.core
  (:require [advent.tools :as tools]
            [advent.grid :as grid]))

(defn next-beam-states [grid [point direction]]
  (let [ch          (grid point)
        horizontal? (#{grid/left grid/right} direction)
        directions  (case ch
                      \. [direction]
                      \/ [({grid/left grid/down
                            grid/right grid/up
                            grid/up grid/right
                            grid/down grid/left} direction)]
                      \\ [({grid/left grid/up
                            grid/right grid/down
                            grid/up grid/left
                            grid/down grid/right} direction)]
                      \- (if horizontal?
                           [direction]
                           [grid/left grid/right])
                      \| (if horizontal?
                           [grid/up grid/down]
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
        start [[0 0] grid/right]]
    (solve grid start)))

(defn part-2 [file]
  (let [grid (tools/read-grid file)
        {:keys [rows cols]} (tools/read-grid-dimensions file)]
    (->> (concat
          (for [x (range cols)]
            [[x 0] grid/down])
          (for [x (range cols)]
            [[x (dec rows)] grid/up])
          (for [y (range rows)]
            [[0 y] grid/right])
          (for [y (range rows)]
            [[(dec cols) y] grid/left]))
         (map (partial solve grid))
         (apply max))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))