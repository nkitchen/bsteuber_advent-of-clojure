(ns advent.year-2024.day-15.core
  (:require
   [advent.tools :as tools]
   [advent.grid :as grid]))

(defn read-input [file]
  (let [[grid-lines cmd-lines] (tools/read-blocks file)
        grid (tools/read-grid-from-lines grid-lines
                                         (fn [ch]
                                           (case ch
                                             \# :wall
                                             \O :box
                                             \@ :start-position
                                             nil)))
        start-position (some (fn [[point content]]
                               (when (= content :start-position)
                                 point))
                             grid)
        grid (dissoc grid start-position)
        directions (->> cmd-lines
                        (mapcat (fn [line]
                                  (map grid/read-direction line)))
                        vec)]
    [[grid start-position] directions]))

(defn move-boxes-1 [grid box-count first-box-pos direction]
  (if (pos? box-count)
    (-> grid
        (dissoc first-box-pos)
        (assoc (grid/go-dir first-box-pos direction box-count)
               :box))
    grid))

(defn move-1 [[grid position] direction]
  (loop [boxes-to-move 0
         current-pos position]
    (let [next-pos (grid/go-dir current-pos direction)]
      (case (grid next-pos)
        :wall [grid position]
        :box (recur (inc boxes-to-move)
                    next-pos)
        (let [next-robot-pos (grid/go-dir position direction)]
          [(move-boxes-1 grid boxes-to-move next-robot-pos direction)
           next-robot-pos])))))
(defn score-grid-1 [grid]
  (->> grid
       (filter (fn [[_ content]]
                 (= content :box)))
       (map first)
       (map (fn [[x y]]
              (+ (* y 100) x)))
       (apply +)))

#_(defn print-state-1 [[grid position]]
    (let [max-x (->> grid
                     (map (comp first first))
                     (apply max))
          max-y (->> grid
                     (map (comp second first))
                     (apply max))]
      (doseq [y (range (inc max-y))]
        (doseq [x (range (inc max-x))]
          (let [point [x y]
                content (grid point)]
            (print (cond
                     (= point position) \@
                     (= content :wall)  \#
                     (= content :box)   \O
                     :else              \.))))
        (println))))

(defn part-1 [file]
  (let [[init-state directions] (read-input file)
        [grid _] (reduce move-1 init-state directions)]
    (score-grid-1 grid)))

(defn move-boxes-2 [grid boxes direction]
  (let [without-boxes (apply dissoc grid boxes)]
    (->> boxes
         (map (fn [box]
                [(grid/go-dir box direction) :box]))
         (into without-boxes))))

(defn print-state-2 [[grid position]]
  (let [max-x (->> grid
                   (map (comp first first))
                   (apply max))
        max-y (->> grid
                   (map (comp second first))
                   (apply max))]
    (doseq [y (range (inc max-y))]
      (doseq [x (range 0 (inc max-x) 1/2)]
        (let [point [x y]
              content (grid point)
              content-left (grid [(- x 1/2) y])]
          (print (cond
                   (= point position) \@
                   (= content :wall)  \#
                   (= content-left :wall) \#
                   (= content :box)   \[
                   (= content-left :box) \]
                   :else              \.))))
      (println))))
(defn move-2 [[grid position] original-direction]
  (let [direction (if (#{grid/east grid/west} original-direction)
                    (grid/times 1/2 original-direction)
                    original-direction)]
    #_(print-state-2 [grid position])
    #_(println "Move" original-direction)
    (loop [boxes-to-move []
           positions [position]]
      (let [next-positions (map #(grid/go-dir % direction)
                                positions)]
        (if (some (fn [[x y :as pos]]
                    (or (= :wall (grid pos))
                        (= :wall (grid [(- x 1/2) y]))))
                  next-positions)
          [grid position]
          (let [boxes (->> next-positions
                           (mapcat (fn [[x y :as pos]]
                                     (let [left-box-pos [(- x 1/2) y]]
                                       [(when (= :box (grid pos))
                                          pos)
                                        (when (= :box (grid left-box-pos))
                                          left-box-pos)])))
                           (filter some?)
                           distinct
                           seq)]
            (if boxes
              (recur (into boxes-to-move boxes)
                     (->> boxes
                          (mapcat (fn [[x y :as box]]
                                    (let [box-right [(+ x 1/2) y]]
                                      (condp = original-direction
                                        grid/north [box box-right]
                                        grid/south [box box-right]
                                        grid/west [box]
                                        grid/east [box-right]))))))
              [(move-boxes-2 grid boxes-to-move direction)
               (grid/go-dir position direction)])))))))

(defn score-grid-2 [grid]
  (->> grid
       (filter (fn [[_ content]]
                 (= content :box)))
       (map first)
       (map (fn [[x y]]
              (+ (* y 100) (* x 2))))
       (apply +)))

(defn part-2 [file]
  (let [[init-state directions] (read-input file)
        [grid _ :as state] (reduce move-2 init-state directions)]
    (score-grid-2 grid)))

(comment
  (part-1 "test1")
  (part-1 "test2")
  (part-1 "input")
  (part-2 "test3")
  (part-2 "test2")
  (part-2 "input"))
