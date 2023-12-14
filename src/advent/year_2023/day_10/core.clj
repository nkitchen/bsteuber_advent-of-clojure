(ns advent.year-2023.day-10.core
  (:require [advent.grid :as grid]
            [advent.tools :as tools]))

(def pipe-mapping
  {\| [grid/up grid/down]
   \- [grid/left grid/right]
   \L [grid/up grid/right]
   \J  [grid/up grid/left]
   \7  [grid/down grid/left]
   \F  [grid/down grid/right]})

(defn read-input [file]
  (->> file
       tools/read-grid
       (reduce (fn [data [point ch]]
                 (case ch
                   \S (assoc data :start point)
                   \. data

                   (->> ch
                        pipe-mapping
                        (mapv (fn [dir]
                                (grid/go-dir point dir)))
                        (assoc-in data [:connections point]))))
               {})))

(defn next-in-path [connections [prev current]]
  (when-let [next (->> (get connections current)
                       (remove #{prev})
                       first)]
    [current next]))

(defn path-from [connections start first-neighb]
  (->> [start first-neighb]
       (iterate (partial next-in-path connections))
       (map first)
       (take-while some?)
       vec))

(defn find-loop [file]
  (let [{:keys [start connections]} (read-input file)
        start-neighb (->> connections
                          (some (fn [[point neighbs]]
                                  (when (some #{start} neighbs)
                                    point))))]
    (path-from connections start start-neighb)))

(defn part-1 [file]
  (let [path (find-loop file)]
    (quot (count path) 2)))

(defn part-2 [file]
  (let [path (find-loop file)
        on-path? (into #{} path)
        path-ups (->> (concat path (take 2 path))
                      (partition 3 1)
                      (map (fn [[[_ prev-y] curr [_ next-y]]]
                             (let [[_ curr-y] curr]
                               [curr
                                (if (some #{(dec curr-y)}
                                          [prev-y next-y])
                                  1
                                  0)])))
                      (into {}))
        min-x (->> path (map first) (apply min))
        max-x (->> path (map first) (apply max))
        min-y (->> path (map second) (apply min))
        max-y (->> path (map second) (apply max))]
    (->> (range min-y max-y)
         (map (fn [y]
                (->> (range min-x max-x)
                     (reduce (fn [[ups enclosed] x]
                               (let [point [x y]]
                                 (if (on-path? point)
                                   [(+ ups (path-ups point)) enclosed]
                                   [ups (if (odd? ups)
                                          (inc enclosed)
                                          enclosed)])))
                             [0 0])
                     second)))
         (apply +))))

(comment
  (part-1 "test")
  (part-1 "test2")
  (part-1 "input")
  (part-2 "test")
  (part-2 "test3")
  (part-2 "input"))