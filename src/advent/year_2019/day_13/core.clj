(ns advent.year-2019.day-13.core
  (:require [advent.year-2019.intcode :as intcode]))

(defn part-1 []
  (let [program (intcode/read-input-program)
        output (:output (intcode/run-until-halt program []))]
    (->> output
         (partition 3)
         (reduce (fn [display [x y tile]]
                   (assoc display [x y] tile))
                 {})
         (filter (fn [[_ tile]]
                   (= tile 2)))
         count)))

(defn print-state [{:keys [display score]}]
  (doseq [y (range 26)]
    (doseq [x (range 46)]
      (print (case (display [x y])
               0 "."
               1 "|"
               2 "#"
               3 "-"
               4 "o")))
    (println))
  (println score))

(defn next-output-state [state]
  (when-let [state (->> state
                        intcode/run-states
                        (drop-while #(< (count (:output %)) 3))
                        first)]
    (let [[x y score-or-tile] (:output state)
          state (assoc state :output [])]
      (if (and (= x -1)
               (= y 0))
        (assoc state :score score-or-tile)
        (let [state (assoc-in state [:display [x y]] score-or-tile)
              state (case score-or-tile
                      3 (assoc state :paddle [x y])
                      4 (assoc state :ball [x y])
                      state)
              ball-paddle-diff (- (first (:ball state [0 0]))
                                  (first (:paddle state [0 0])))
              cmd (cond (pos? ball-paddle-diff) 1
                        (neg? ball-paddle-diff) -1
                        :else 0)
              state (assoc state :input
                           (repeat cmd))]
          state)))))

(defn part-2 []
  (let [program (-> (intcode/read-input-program)
                    (assoc 0 2))
        input (repeat 1000 0)
        start (->> (intcode/init-state program input)
                   (iterate next-output-state)
                   (drop-while (comp nil? :score))
                   first)]
    (->> start
         (iterate next-output-state)
         (take-while some?)
         last
         print-state)))

(comment
  (part-1)
  (part-2))