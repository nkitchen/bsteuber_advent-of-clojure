(ns advent.year-2024.day-14.core
  (:require
   [advent.tools :as tools]
   [advent.grid :as grid]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (let [[px py vx vy] (->> line
                                        (re-seq #"-?\d+")
                                        (map tools/read-int))]
                 [[px py] [vx vy]])))))

(defn simulate [steps [sizex sizey] [[px py] [vx vy]]]
  (let [x (mod (+ px (* vx steps)) sizex)
        y (mod (+ py (* vy steps)) sizey)]
    [x y]))

(defn assign-quadrant [[sizex sizey] [px py]]
  (let [middlex (/ (dec sizex) 2)
        middley (/ (dec sizey) 2)]
    (when-not (or (= px middlex)
                  (= py middley))
      [(> px middlex)
       (> py middley)])))

(defn part-1 [file size]
  (->> file
       read-input
       (map (partial simulate 100 size))
       (map (partial assign-quadrant size))
       (filter some?)
       frequencies
       vals
       (apply *)))

(defn print-bots [bots [sizex sizey]]
  (let [bot-set (into #{} bots)]
    (doseq [y (range sizey)]
      (doseq [x (range sizex)]
        (print (if (bot-set [x y])
                 "*"
                 " ")))
      (println))))

(defn could-be-xmas-tree? [bots _]
  (let [bot-set (into #{} bots)]
    (->> bot-set
         (filter (fn [bot]
                   (->> grid/directions-4
                        (map (partial grid/go-dir bot))
                        (some bot-set))))
         count
         (< (* 0.5 (count bot-set))))))

(defn part-2 [file size]
  (let [bots (read-input file)]
    (->> (range (apply * size))
         (some (fn [steps]
                 (let [bots (map (partial simulate steps size)
                                 bots)]
                   (when (could-be-xmas-tree? bots size)
                     (println "Steps:" steps)
                     (print-bots bots size)
                     true)))))))

(comment
  (part-1 "test" [11 7])
  (part-1 "input" [101 103])
  (part-2 "input" [101 103]))
