(ns advent.year-2021.day-05.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-point [point]
  (->> (str/split point #",")
       (mapv tools/read-int)))

(defn read-line-def [line]
  (->> (str/split line #" -> ")
       (mapv read-point)))


(defn read-input [file]
  (->> file
       (tools/read-lines)
       (mapv read-line-def)))

(defn horizontal-or-vertical? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2)
      (= y1 y2)))

(defn range-including [x1 x2]
  (let [smaller (min x1 x2)
        larger (max x1 x2)
        res (range smaller (inc larger))]
    (if (= smaller x1)
      res
      (reverse res))))

(defn line-points [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (->> (range-including y1 y2)
                   (mapv #(vector x1 %)))
    (= y1 y2) (->> (range-including x1 x2)
                   (mapv #(vector % y1)))
    :else (mapv vector
                (range-including x1 x2)
                (range-including y1 y2))))

(defn part-1 [file]
  (->>
   (read-input file)
   (filter horizontal-or-vertical?)
   (mapcat line-points)
   frequencies
   (filter (fn [[_ freq]]
             (>= freq 2)))
   count))

(defn part-2 [file]
  (->>
   (read-input file)
   (mapcat line-points)
   frequencies
   (filter (fn [[_ freq]]
             (>= freq 2)))
   count))

(comment
  (line-points [[0 8] [8 0]])
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))