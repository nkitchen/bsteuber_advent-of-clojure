(ns advent.year-2023.day-07.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (map (fn [line]
              (let [[hand bet] (str/split line #" ")]
                [hand (tools/read-int bet)])))))

(def ordered-cards-part-1 "23456789TJQKA")

(def card-strength-part-1
  (->> ordered-cards-part-1
       (map-indexed (fn [i card]
                      [card i]))
       (into {})))

(defn top-card-frequencies [hand]
  (let [[first-kind second-kind] (->> hand
                                      frequencies
                                      (map val)
                                      (sort >))]
    [(or first-kind 0)
     (or second-kind 0)]))

(def card-frequency-strength
  {[1 1] 0
   [2 1] 1
   [2 2] 2
   [3 1] 3
   [3 2] 4
   [4 1] 5
   [5 0] 6})

(defn hand-strength-part-1 [hand]
  (card-frequency-strength (top-card-frequencies hand)))

(def ordered-cards-part-2 "J23456789TQKA")

(def card-strength-part-2
  (->> ordered-cards-part-2
       (map-indexed (fn [i card]
                      [card i]))
       (into {})))

(defn hand-strength-part-2 [hand]
  (let [[first-kind second-kind] (-> hand
                                     (str/replace "J" "")
                                     top-card-frequencies)
        joker-count (->> hand
                         (filter #{\J})
                         count)]
    (card-frequency-strength [(+ first-kind joker-count)
                              second-kind])))

(defn solve [part-2? file]
  (let [[hand-strength
         card-strength] (if part-2?
                          [hand-strength-part-2 card-strength-part-2]
                          [hand-strength-part-1 card-strength-part-1])]
    (->> file
         read-input
         (map (fn [[hand bet]]
                [(into [(hand-strength hand)]
                       (map card-strength hand))
                 bet]))
         (sort-by first)
         (map-indexed (fn [i [_ bet]]
                        (* (inc i) bet)))
         (apply +))))

(def part-1 (partial solve false))
(def part-2 (partial solve true))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))