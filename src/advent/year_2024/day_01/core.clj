(ns advent.year-2024.day-01.core
  (:require
   [advent.tools :as tools]
   [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (->> (str/split line #"\s+")
                    (mapv tools/read-int))))))

(defn part-1 [file]
  (->> file
       read-input
       (apply map vector)
       (map sort)
       (apply map (fn [x y]
                    (abs (- x y))))
       (apply +)))

(defn part-2 [file]
  (let [[left right] (->> file
                          read-input
                          (apply map vector))
        count-appearences (frequencies right)]
    (->> left
         (map (fn [x]
                (* x (or (count-appearences x) 0))))
         (apply +))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
