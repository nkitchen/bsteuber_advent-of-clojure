(ns advent.year-2019.day-05.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]
            [advent.year-2019.intcode :as intcode]))

(defn part-1 []
  (let [output (-> (intcode/read-input-program)
                   (intcode/run-until-halt [1])
                   :output)]
    (assert (every? zero? (butlast output)))
    (last output)))

(defn part-2 []
  (-> (intcode/read-input-program)
      (intcode/run-until-halt [5])
      :output
      first))

(comment
  (part-1)
  (part-2))