(ns advent.year-2023.day-02.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-game [line]
  (let [[game cube-sets] (str/split line #": ")
        game-id (tools/read-int (second (str/split game #" ")))]
    [game-id
     (->> (str/split cube-sets #"; ")
          (mapv (fn [cube-set]
                  (->> (str/split cube-set #", ")
                       (map (fn [cube-amount]
                              (let [[number color] (str/split cube-amount #" ")]
                                [color (tools/read-int number)])))
                       (into {})))))]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (map read-game)
       (into (sorted-map))))

(defn amounts<= [amounts-1 amounts-2]
  (every? (fn [[color number]]
            (<= number (get amounts-2 color 0)))
          amounts-1))

(defn part-1 [file]
  (let [max-amounts {"red" 12
                     "green" 13
                     "blue" 14}]
    (->> file
         read-input
         (filter (fn [[_ cube-sets]]
                   (every? #(amounts<= % max-amounts)
                           cube-sets)))
         (map key)
         (apply +))))

(defn max-amounts [amounts-1 amounts-2]
  (merge-with max amounts-1 amounts-2))

(defn part-2 [file]
  (->> file
       read-input
       (map (fn [[_ cube-sets]]
              (reduce max-amounts {} cube-sets)))
       (map (fn [amounts]
              (->> amounts
                   vals
                   (apply *))))
       (apply +)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))