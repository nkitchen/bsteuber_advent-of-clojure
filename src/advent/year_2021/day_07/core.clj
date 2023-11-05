(ns advent.year-2021.day-07.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> (str/split (->> file
                       (tools/read-lines)
                       first)
                  #",")
       (mapv tools/read-int)))

(defn abs-diff [target-pos pos]
  (Math/abs (- target-pos pos)))

(def fuel-required-part-1 abs-diff)

(defn fuel-required-part-2 [target-pos pos]
  (let [diff (abs-diff target-pos pos)]
    (/ (* diff (inc diff)) 2)))

(defn total-fuel-required [target-pos positions fuel-required-fn]
  (->> positions
       (map #(fuel-required-fn target-pos %))
       (apply +)))

(defn solve [file fuel-required-fn]
  (let [positions (read-input file)
        min-pos (apply min positions)
        max-pos (apply max positions)]
    (->> (range min-pos (inc max-pos))
         (map (fn [target-pos]
                (total-fuel-required target-pos positions fuel-required-fn)))
         (apply min))))

(defn part-1 [file]
  (solve file abs-diff))

(defn part-2 [file]
  (solve file fuel-required-part-2))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))