(ns advent.year-2019.day-02.core
  (:require [advent.year-2019.intcode :as intcode]))

(defn run [program noun verb]
  (-> program
      (assoc 1 noun)
      (assoc 2 verb)
      (intcode/run-until-halt [])
      :program
      first))

(defn part-1 []
  (run (intcode/read-input-program)
       12 2))

(defn part-2 []
  (let [program (intcode/read-input-program)]
    (->> (for [noun (range 100)
               verb (range 100)]
           [noun verb])
         (some (fn [[noun verb]]
                 (when (= (run program noun verb)
                          19690720)
                   (+ (* noun 100) verb)))))))

(comment
  (part-1)
  (part-2))