(ns advent.year-2024.day-06.core
  (:require
   [advent.tools :as tools]
   [advent.grid :as grid]))

(defn read-input [file]
  (let [grid (tools/read-grid file (fn [ch]
                                     (case ch
                                       \. :empty
                                       \^ :start-position
                                       \# :obstruction)))
        start-position (some (fn [[point k]]
                               (when (= k :start-position)
                                 point))
                             grid)]
    {:grid (assoc grid start-position :empty)
     :position [start-position grid/north]}))

(defn next-position [grid [point direction]]
  (let [point-ahead (grid/go-dir point direction)]
    (case (grid point-ahead)
      :empty [point-ahead direction]
      :obstruction [point (grid/rotate-right direction)]
      nil)))

(defn path-points [grid position]
  (->> (iterate (partial next-position grid) position)
       (take-while some?)
       (map first)
       distinct))

(defn part-1 [file]
  (let [{:keys [grid position]} (read-input file)]
    (count (path-points grid position))))

(defn has-loop? [grid position]
  (loop [position position
         visited #{position}]
    (let [next-pos (next-position grid position)]
      (cond
        (nil? next-pos)    false
        (visited next-pos) true
        :else              (recur next-pos (conj visited next-pos))))))

(defn part-2 [file]
  (let [{:keys [grid position]} (read-input file)]
    (->> (path-points grid position)
         (remove #{position})
         (filter (fn [obstruct-position]
                   (has-loop? (assoc grid obstruct-position :obstruction)
                              position)))
         count)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
