(ns advent.year-2019.day-01.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv tools/read-int)))

(defn calc-fuel [mass]
  (-> mass
      (quot 3)
      (- 2)))

(defn part-1 []
  (->> "input"
       read-input
       (map calc-fuel)
       (apply +)))

(defn calc-total-fuel [mass]
  (loop [total-fuel 0
         mass mass]
    (let [fuel (calc-fuel mass)]
      (if (pos? fuel)
        (recur (+ total-fuel fuel) fuel)
        total-fuel))))

(defn part-2 []
  (->> "input"
       read-input
       (map calc-total-fuel)
       (apply +)))

(comment
  (part-1)
  (part-2))