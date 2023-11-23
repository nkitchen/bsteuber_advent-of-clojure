(ns advent.year-2019.day-11.core
  (:require [advent.year-2019.intcode :as intcode]))

(defn turn-right [dir]
  (case dir
    :left :up
    :up :right
    :right :down
    :down :left))

(defn turn-left [dir]
  (case dir
    :up :left
    :right :up
    :down :right
    :left :down))

(defn go-forward [{:keys [dir]
                   :as state}]
  (case dir
    :up (update state :y dec)
    :down (update state :y inc)
    :left (update state :x dec)
    :right (update state :x inc)))

(defn next-state [{:keys [x y] :as state}]
  (when-let [state (->> (iterate intcode/step state)
                        (take-while some?)
                        (drop-while (fn [state]
                                      (< (count (:output state))
                                         2)))
                        first)]
    (let [[paint-color turn] (:output state)
          turn-fn (case turn
                    0 turn-left
                    1 turn-right)
          state (assoc-in state [:grid [x y]] paint-color)
          state (update state :dir turn-fn)
          state (go-forward state)
          {:keys [x y grid]} state]
      (-> state
          (assoc :output [])
          (assoc :input [(get grid [x y] 0)])))))

(defn run [initial-panel]
  (let [program (intcode/read-input-program)
        init-state (assoc (intcode/init-state program [initial-panel])
                          :x 0
                          :y 0
                          :dir :up
                          :grid {[0 0] initial-panel})]
    (->> init-state
         (iterate next-state)
         (take-while some?)
         last
         :grid)))

(defn part-1 []
  (count (run 0)))

(defn part-2 []
  (let [grid (run 1)
        xs (map (comp first key) grid)
        min-x (apply min xs)
        max-x (apply max xs)
        ys (map (comp first key) grid)
        min-y (apply min ys)
        max-y (apply max ys)]
    (doseq [y (range min-y (inc max-y))]
      (doseq [x (range min-x (inc max-x))]
        (print (case (get grid [x y] 0)
                 0 " "
                 1 "#")))
      (println))))

(comment
  (part-1)
  (part-2))