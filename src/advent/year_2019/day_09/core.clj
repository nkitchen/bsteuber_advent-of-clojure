(ns advent.year-2019.day-09.core
  (:require [advent.year-2019.intcode :as intcode]))

(defn part-1 []
  (-> (intcode/read-input-program)
      (intcode/run-until-halt [1])
      :output))

(defn part-2 []
  (-> (intcode/read-input-program)
      (intcode/run-until-halt [2])
      :output))

(comment
  (part-1)
  (part-2))