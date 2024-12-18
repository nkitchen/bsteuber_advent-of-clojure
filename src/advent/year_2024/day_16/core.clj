(ns advent.year-2024.day-16.core
  (:require
   [advent.grid :as grid]
   [advent.tools :as tools]
   [clojure.data.priority-map :refer [priority-map]]))

(defn read-input [file]
  (let [grid (tools/read-grid file (fn [ch]
                                     (case ch
                                       \# :wall
                                       \S :start
                                       \E :end
                                       nil)))
        start (some (fn [[point content]]
                      (when (= content :start)
                        point))
                    grid)
        grid (dissoc grid start)]
    [grid [start grid/east]]))

(defn transitions [grid [position direction]]
  (filter some?
          [(let [next-pos (grid/go-dir position direction)]
             (when-not (= :wall (grid next-pos))
               [[next-pos direction] 1]))
           [[position (grid/rotate-left direction)] 1000]
           [[position (grid/rotate-right direction)] 1000]]))

(defn part-1 [file]
  (let [[grid init-state] (read-input file)]
    (loop [explore (priority-map init-state 0)
           seen #{}]
      (let [[[postition _ :as state] cost] (first explore)]
        (if (= :end (grid postition))
          cost
          (let [seen-next (conj seen state)
                explore-next (->> state
                                  (transitions grid)
                                  (map (fn [[state transition-cost]]
                                         [state (+ cost transition-cost)]))
                                  (reduce (fn [explore [state cost]]
                                            (if (or (seen-next state)
                                                    (when-let [existing-cost (explore state)]
                                                      (<= existing-cost cost)))
                                              explore
                                              (assoc explore state cost)))
                                          explore))]
            (recur (dissoc explore-next state)
                   seen-next)))))))

(defn back-transitions [[position direction]]
  (filter some?
          [(let [next-pos (grid/go-dir position direction -1)]
             [[next-pos direction] 1])
           [[position (grid/rotate-left direction)] 1000]
           [[position (grid/rotate-right direction)] 1000]]))
(defn count-seats [final-states known-best-cost]
  (loop [explore final-states
         seats (into #{} (map first final-states))]
    (if (empty? explore)
      (count seats)
      (let [state (first explore)
            cost (known-best-cost state)
            next-states (->> (back-transitions state)
                             (filter (fn [[prev-state transition-cost]]
                                       (when-let [prev-cost (known-best-cost prev-state)]
                                         (= cost (+ prev-cost transition-cost)))))
                             (map first))]
        (recur (into (rest explore) next-states)
               (conj seats (first state)))))))

(defn part-2 [file]
  (let [[grid init-state] (read-input file)]
    (loop [explore (priority-map init-state 0)
           known-best-cost {}]
      (let [[[postition _ :as state] cost] (first explore)]
        (if (= :end (grid postition))
          (let [final-states (->> explore
                                  (take-while (fn [[_ state-cost]]
                                                (= state-cost cost)))
                                  (map first)
                                  (filter (fn [[position _]]
                                            (= :end (grid position)))))
                known-best-cost (->> final-states
                                     (map (fn [state]
                                            [state cost]))
                                     (into known-best-cost))]
            (count-seats final-states known-best-cost))
          (let [seen-next (assoc known-best-cost state cost)
                explore-next (->> state
                                  (transitions grid)
                                  (map (fn [[state transition-cost]]
                                         [state (+ cost transition-cost)]))
                                  (reduce (fn [explore [state cost]]
                                            (if (or (seen-next state)
                                                    (when-let [existing-cost (explore state)]
                                                      (<= existing-cost cost)))
                                              explore
                                              (assoc explore state cost)))
                                          explore))]
            (recur (dissoc explore-next state)
                   seen-next)))))))

(comment
  (part-1 "test1")
  (part-1 "test2")
  (part-1 "input")
  (part-2 "test1")
  (part-2 "test2")
  (part-2 "input"))
