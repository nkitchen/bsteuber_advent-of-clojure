(ns advent.year-2021.day-02.core
  (:require [clojure.string :as str]
            [advent.tools :as tools]))

(defn read-input [file]
  (->> (tools/read-lines file)
       (mapv (fn [line]
               (let [[direction steps] (str/split line #" ")]
                 [(keyword direction) (tools/read-int steps)])))))

(defn go-direction-part-1 [[pos depth] [direction steps]]
  (case direction
    :forward [(+ pos steps) depth]
    :down [pos (+ depth steps)]
    :up [pos (- depth steps)]))

(defn part-1 [file]
  (->> (read-input file)
       (reduce go-direction-part-1 [0 0])
       (apply *)))

(defn go-direction-part-2 [[pos depth aim] [direction steps]]
  (case direction
    :forward [(+ pos steps) (+ depth (* aim steps)) aim]
    :down [pos depth (+ aim steps)]
    :up [pos depth (- aim steps)]))

(defn part-2 [file]
  (->> (read-input file)
       (reduce go-direction-part-2 [0 0 0])
       butlast
       (apply *)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
