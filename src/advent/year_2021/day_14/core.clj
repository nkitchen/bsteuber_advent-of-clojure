(ns advent.year-2021.day-14.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (let [[state rules] (tools/read-blocks file)]
    {:state (first state)
     :rules (->> rules
                 (map (fn [line]
                        (vec (str/split line #" -> "))))
                 (into {}))}))

(defn initial-pair-freqs [state]
  (->> state
       (partition 2 1)
       (map #(apply str %))
       frequencies))

(defn next-state [rules pair-freqs]
  (->> pair-freqs
       (mapcat (fn [[pair freq]]
                 (if-let [insert (rules pair)]
                   [[(str (first pair) insert) freq]
                    [(str insert (second pair)) freq]]
                   [[pair freq]])))
       (reduce (fn [pair-freqs [pair freq]]
                 (update pair-freqs pair (fnil + 0) freq))
               {})))

(defn run [file steps]
  (let [{:keys [rules state]} (read-input file)
        pair-freqs (->> state
                        initial-pair-freqs
                        (iterate (partial next-state rules))
                        (drop steps)
                        first)
        letter-freqs (->> pair-freqs
                          (mapcat (fn [[pair freq]]
                                    [[(first pair) freq]
                                     [(second pair) freq]]))
                          (reduce (fn [freqs [letter freq]]
                                    (update freqs letter (fnil + 0) freq))
                                  {}))
        letter-freqs (-> letter-freqs
                         (update (first state) inc)
                         (update (last state) inc)
                         (update-vals #(/ % 2)))
        sorted-letter-freqs (sort (vals letter-freqs))]
    (- (last sorted-letter-freqs)
       (first sorted-letter-freqs))))

(defn part-1 [file]
  (run file 10))

(defn part-2 [file]
  (run file 40))

(comment
  (initial-pair-freqs "NBBBCNCCNBBNBNBBCHBHHBCHB")
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))