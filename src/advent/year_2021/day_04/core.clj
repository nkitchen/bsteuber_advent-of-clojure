(ns advent.year-2021.day-04.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-numbers [numbers]
  (->> (str/split numbers #",")
       (mapv tools/read-int)))

(defn read-board [lines]
  (->> lines
       (map-indexed
        (fn [row line]
          (->> (str/split line #" ")
               (filter seq)
               (map-indexed (fn [col num]
                              [[row col] (tools/read-int num)])))))
       (apply concat)
       (into {})))

(defn read-input [file]
  (let [[[numbers] & boards] (->> file
                                  tools/read-blocks)]
    {:numbers (read-numbers numbers)
     :boards (map read-board boards)}))

(def all-lines
  (concat (for [row (range 5)]
            (for [col (range 5)]
              [row col]))
          (for [col (range 5)]
            (for [row (range 5)]
              [row col]))))

(defn won? [marked]
  (some (fn [line]
          (every? marked line))
        all-lines))

(defn draw-number [boards marks number]
  (let [marks (map (fn [board marked]
                     (let [position (some (fn [[pos num]]
                                            (when (= num number)
                                              pos))
                                          board)]
                       (if position
                         (conj marked position)
                         marked)))
                   boards
                   marks)
        winner-indices (->> marks
                            (map-indexed (fn [i marked]
                                           (when (won? marked)
                                             i)))
                            (filter some?)
                            seq)]
    [marks (when winner-indices
             (into #{} winner-indices))]))

(defn final-score [boards marks winner-index number]
  (let [board (nth boards winner-index)
        marked (nth marks winner-index)]
    (->> board
         (remove (fn [[pos _]]
                   (marked pos)))
         (map val)
         (apply +)
         (* number))))

(defn part-1 [file]
  (let [{:keys [numbers boards]} (read-input file)]
    (loop [numbers numbers
           marks (repeat (count boards) #{})]
      (let [number (first numbers)
            _ (assert number)
            [marks winner-indices] (draw-number boards marks number)]
        (if winner-indices
          (final-score boards marks (first winner-indices) number)
          (recur (next numbers)
                 marks))))))

(defn remove-indices [xs indices]
  (->> xs
       (map-indexed (fn [i x]
                      (when-not (indices i)
                        x)))
       (filter some?)))

(defn part-2 [file]
  (let [{:keys [numbers boards]} (read-input file)]
    (loop [numbers numbers
           boards boards
           marks (repeat (count boards) #{})]
      (let [number (first numbers)
            _ (when-not number
                (prn (first boards))
                (throw (RuntimeException. "Out of numbers")))
            [marks winner-indices] (draw-number boards marks number)]
        (if winner-indices
          (if (= 1 (count boards))
            (final-score boards marks (first winner-indices) number)
            (recur (next numbers)
                   (remove-indices boards winner-indices)
                   (remove-indices marks winner-indices)))
          (recur (next numbers)
                 boards
                 marks))))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))