(ns advent.year-2024.day-04.core
  (:require
   [advent.tools :as tools]
   [advent.grid :as grid]))

(defn read-input [file]
  (merge {:grid (tools/read-grid file)}
         (tools/read-grid-dimensions file)))

(defn part-1 [file]
  (let [{:keys [grid]
         :as dimensions} (read-input file)]
    (->> (grid/all-points dimensions)
         (map (fn [start-point]
                (->> grid/directions-8
                     (filter (fn [dir]
                               (->> (range 4)
                                    (map (fn [steps]
                                           (grid/go-dir start-point dir steps)))
                                    (map grid)
                                    (= (seq "XMAS")))))
                     count)))
         (apply +))))

(defn part-2 [file]
  (let [{:keys [grid]
         :as dimensions} (read-input file)]
    (->> (grid/all-points dimensions)
         (filter (fn [start-point]
                   (->> grid/diagonals
                        (filter (fn [dir]
                                  (->> (range -1 2)
                                       (map (fn [steps]
                                              (grid/go-dir start-point dir steps)))
                                       (map grid)
                                       (= (seq "MAS")))))
                        count
                        (= 2))))
         count)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
