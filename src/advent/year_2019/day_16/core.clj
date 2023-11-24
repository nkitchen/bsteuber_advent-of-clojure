(ns advent.year-2019.day-16.core
  (:require [advent.tools :as tools]))

(defn read-nums [s]
  (mapv tools/read-int s))

(def basic-pattern [0 1 0 -1])

(def pattern-for
  (memoize
   (fn [index]
     (->> basic-pattern
          (repeat (inc index))
          (apply mapcat list)
          vec))))

(defn transform [nums]
  (->> (range (count nums))
       (mapv (fn [index]
               (let [pattern (->> index
                                  pattern-for
                                  cycle
                                  next)]
                 (mod
                  (->> (map * nums pattern)
                       (apply +)
                       (#(Math/abs %)))
                  10))))))

(def input (first (tools/read-lines "input")))

(defn part-1 [s]
  (->> (nth (->> s
                 read-nums
                 (iterate transform))
            100)
       (take 8)
       (apply str)
       println))

(defn fast-transform [from-end]
  (reductions (fn [x y]
                (mod (+ x y) 10)) from-end))

(defn part-2 [s]
  (let [start-index (tools/read-long (subs s 0 7))]
    (->> s
         read-nums
         (repeat 10000)
         (apply concat)
         (drop start-index)
         reverse
         (iterate fast-transform)
         (drop 100)
         first
         reverse
         (take 8)
         (apply str)
         println)))

(comment
  (part-1 "80871224585914546619083218645595")
  (part-1 input)
  (part-2 input))