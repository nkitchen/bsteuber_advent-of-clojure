(ns advent.year-2021.day-01.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  (->> (tools/read-lines file)
       (mapv tools/read-int)))

(defn part-1 [file]
  (->> file
       read-input
       (partition 2 1)
       (filter (fn [[x y]]
                 (> y x)))
       count))

(defn part-2 [file]
  (->> file
       read-input
       (partition 3 1)
       (map #(apply + %))
       (partition 2 1)
       (filter (fn [[x y]]
                 (> y x)))
       count))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))