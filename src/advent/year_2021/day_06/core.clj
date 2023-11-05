(ns advent.year-2021.day-06.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> (str/split (->> file
                       tools/read-lines
                       first)
                  #",")
       (map tools/read-int)
       frequencies))

(defn next-state [freqs]
  (->> freqs
       (map (fn [[days freq]]
              (if (= days 0)
                {6 freq, 8 freq}
                {(dec days) freq})))
       (apply merge-with +)))

(defn run [file days]
  (->> file
       read-input
       (iterate next-state)
       (drop days)
       first
       (map second)
       (apply +)))

(comment
  (run "test" 18)
  (run "test" 80)
  (run "input" 80)
  (run "test" 256)
  (run "input" 256))