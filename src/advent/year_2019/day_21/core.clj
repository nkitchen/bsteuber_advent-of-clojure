(ns advent.year-2019.day-21.core
  (:require [advent.year-2019.intcode :as intcode]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]))

(defn run-script [cmds]
  (let [input-str (str (->> cmds
                            (str/join "\n"))
                       "\n")
        input (mapv int input-str)
        output (:output (intcode/run-until-halt
                         (intcode/read-input-program)
                         input))]
    (if (> (last output)
           1000)
      (last output)
      (println (apply str (map char output))))))

(defn part-1 []
  (run-script
   ["NOT A J"
    "NOT B T"
    "OR T J"
    "NOT C T"
    "OR T J"
    "AND D T"
    "WALK"]))

(defn part-2 []
  (run-script
   ["NOT C J"
    "AND D J"
    "AND H J"
    "NOT B T"
    "AND D T"
    "OR T J"
    "NOT A T"
    "OR T J"
    "RUN"]))

(comment
  (part-1)
  (part-2))