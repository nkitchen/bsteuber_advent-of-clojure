(ns advent.year-2023.day-09.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (->> (str/split line #" ")
                    (mapv tools/read-int))))))

(defn diff-seq [xs]
  (when-not (apply = xs)
    (->> xs
         (partition 2 1)
         (mapv (fn [[x y]]
                 (- y x))))))

(defn next-element [xs]
  (if-let [deltas (diff-seq xs)]
    (+ (peek xs)
       (next-element deltas))
    (peek xs)))

(defn part-1 [file]
  (->> file
       read-input
       (map next-element)
       (apply +)))

(defn prev-element [xs]
  (if-let [deltas (diff-seq xs)]
    (- (first xs)
       (prev-element deltas))
    (first xs)))

(defn part-2 [file]
  (->> file
       read-input
       (map prev-element)
       (apply +)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))