(ns advent.year-2023.day-14.core
  (:require [advent.tools :as tools]
            [advent.grid :as grid]))

(defn read-input [file]
  (let [lines (tools/read-lines file)
        rows (count lines)
        cols (count (first lines))]
    (->> (tools/read-grid file #{\# \O})
         (reduce (fn [data [point ch]]
                   (let [k (case ch
                             \# :fixed
                             \O :moveable)]
                     (update data k conj point)))
                 {:rows rows
                  :cols cols
                  :fixed #{}
                  :moveable #{}}))))

(defn blocked? [{:keys [rows cols fixed]} [x y :as point]]
  (or (fixed point)
      (not (and (< -1 x cols)
                (< -1 y rows)))))

(defn place-rock-line [state blocked-point num-rocks direction]
  (->> (iterate #(grid/go-dir % direction)
                blocked-point)
       next
       (take num-rocks)
       (update state :moveable into)))

(defn move-line [state point direction]
  (loop [state state
         point point
         num-rocks 0]
    (if (blocked? state point)
      (place-rock-line state point num-rocks (grid/opposite-dir
                                              direction))
      (let [moveable? (get-in state [:moveable point])
            next-point (grid/go-dir point direction)]
        (if moveable?
          (recur (-> state
                     (update :moveable disj point)
                     (update :todo disj point))
                 next-point
                 (inc num-rocks))
          (recur state
                 next-point
                 num-rocks))))))

(defn tilt [state direction]
  (loop [state (assoc state :todo (:moveable state))]
    (if-let [point (first (:todo state))]
      (recur (move-line state point direction))
      state)))

(defn print-state [{:keys [rows cols fixed moveable]}]
  (dotimes [y rows]
    (dotimes [x cols]
      (print (cond
               (fixed [x y])    "#"
               (moveable [x y]) "O"
               :else            ".")))
    (println)))

(defn count-north-load [{:keys [moveable rows]}]
  (->> moveable
       (map (fn [[_ y]]
              (- rows y)))
       (apply +)))

(defn part-1 [file]
  (let [init-state (read-input file)
        result (tilt init-state grid/up)]
    (count-north-load result)))

(defn run-cycle [state]
  (-> state
      (tilt grid/up)
      (tilt grid/left)
      (tilt grid/down)
      (tilt grid/right)))

(defn part-2 [file]
  (let [init-state (read-input file)
        [cycle-start cycle-end cycle-start-state]
        (loop [steps 0
               seen {}
               state init-state]
          (if (> steps 100000)
            (throw (ex-info "no cycle found" {}))
            (let [{:keys [moveable]} state]
              (if-let [seen-at-steps (seen moveable)]
                [seen-at-steps steps state]
                (recur (inc steps)
                       (assoc seen moveable steps)
                       (run-cycle state))))))
        cycle-len (- cycle-end cycle-start)
        pos-in-cycle (mod (- 1000000000
                             cycle-start)
                          cycle-len)
        final-state (nth (iterate run-cycle cycle-start-state)
                         pos-in-cycle)]
    (count-north-load final-state)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))