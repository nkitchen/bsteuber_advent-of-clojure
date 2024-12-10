(ns advent.year-2024.day-10.core
  (:require
   [advent.tools :as tools]
   [advent.grid :as grid]))

(defn read-input [file]
  (tools/read-grid file tools/read-int))

(defn trail-heads [grid]
  (->> grid
       (map (fn [[point x]]
              (when (zero? x)
                point)))
       (filter some?)))

(defn calc-score [grid start]
  (let [rec-calc (fn rec-calc [number points]
                   (cond
                     (= number 9)
                     (count points)

                     (empty? points)
                     0

                     :else
                     (->> points
                          (mapcat grid/neighbors-4)
                          distinct
                          (filter #(= (grid %) (inc number)))
                          (rec-calc (inc number)))))]
    (rec-calc 0 [start])))

(defn part-1 [file]
  (let [grid (read-input file)]
    (->> (trail-heads grid)
         (map (partial calc-score grid))
         (apply +))))

(defn calc-rating [grid start]
  (let [cache (atom {})
        rec-calc (fn rec-calc [number point]
                   (or (@cache point)
                       (let [trails (if (= number 9)
                                      1
                                      (->> point
                                           grid/neighbors-4
                                           (filter #(= (grid %) (inc number)))
                                           (map (partial rec-calc (inc number)))
                                           (apply +)))]
                         (swap! cache assoc point trails)
                         trails)))]
    (rec-calc 0 start)))

(defn part-2 [file]
  (let [grid (read-input file)]
    (->> (trail-heads grid)
         (map (partial calc-rating grid))
         (apply +))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
