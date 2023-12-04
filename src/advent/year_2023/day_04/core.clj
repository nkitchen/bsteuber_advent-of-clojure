(ns advent.year-2023.day-04.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (let [[_ all-numbers] (str/split line #": ")]
                 (->> (str/split all-numbers #" \| ")
                      (mapv (fn [numbers]
                              (->> (str/split numbers #" ")
                                   (map tools/read-int)
                                   (filter some?)
                                   (into #{}))))))))))

(defn count-winners [[winning-numbers my-numbers]]
  (count (set/intersection winning-numbers my-numbers)))

(defn part-1 [file]
  (->> file
       read-input
       (map (fn [card]
              (let [winners (count-winners card)]
                (if (pos? winners)
                  (int (Math/pow 2 (dec winners)))
                  0))))
       (apply +)))

(defn part-2 [file]
  (let [card-winners (mapv count-winners (read-input file))
        num-cards (count card-winners)]
    (loop [hand (vec (repeat num-cards 1))
           current-card 0]
      (if (< current-card num-cards)
        (let [winners (card-winners current-card)
              next-card (inc current-card)
              won-cards (take winners (range next-card num-cards))
              won-copies (get hand current-card)
              next-hand (reduce (fn [hand won-card]
                                  (update hand won-card + won-copies))
                                hand
                                won-cards)]

          (recur next-hand next-card))
        (apply + hand)))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))