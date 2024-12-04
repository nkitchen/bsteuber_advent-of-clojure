(ns advent.year-2024.day-03.core
  (:require
   [advent.tools :as tools]))

(defn read-input [file]
  (tools/read-lines file))

(def command-regex-part-1
  #"mul\((\d+),(\d+)\)")

(defn apply-command-part-1 [[_ x y]]
  (* (tools/read-int x)
     (tools/read-int y)))

(defn part-1 [file]
  (->> file
       read-input
       (mapcat #(re-seq command-regex-part-1 %))
       (map apply-command-part-1)
       (apply +)))

(def command-regex-part-2
  #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)")

(defn reduce-command-part-2 [[sum enabled?] [s x y]]
  (case s
    "do()" [sum true]
    "don't()" [sum false]
    (let [res (if enabled?
                (apply-command-part-1 [s x y])
                0)]
      [(+ sum res) enabled?])))

(defn part-2 [file]
  (->> file
       read-input
       (mapcat #(re-seq command-regex-part-2 %))
       (reduce reduce-command-part-2 [0 true])
       first))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test2")
  (part-2 "input"))
