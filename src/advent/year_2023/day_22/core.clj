(ns advent.year-2023.day-22.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn brick-z [[[_ _ z] _]]
  z)

(defn read-input [file]
  (->> file
       tools/read-lines
       (map (fn [line]
              (->> (str/split line #"~")
                   (mapv (fn [s]
                           (->> (str/split s #",")
                                (mapv tools/read-int)))))))
       (sort-by brick-z)
       vec))

(defn brick-points [[start end]]
  (let [[sx sy sz] start
        [ex ey ez] end]
    (for [x (range sx (inc ex))
          y (range sy (inc ey))
          z (range sz (inc ez))]
      [x y z])))

(defn xy-projection [points]
  (->> points
       (map pop)
       (into #{})))

(defn find-supporting [height-map xy-points]
  (let [supporting (->> xy-points
                        (map height-map)
                        (filter some?)
                        distinct
                        (sort-by second >)
                        (partition-by second)
                        first)
        height (or (second (first supporting)) 0)
        ids (mapv first supporting)]
    [height ids]))

(defn add-brick [[height-map supported-by] id brick]
  (let [points (brick-points brick)
        xy-points (xy-projection points)
        original-z (brick-z brick)
        [height ids] (find-supporting height-map xy-points)
        new-z (inc height)
        delta (- new-z original-z)
        supported-by (assoc supported-by id ids)
        height-map (reduce (fn [height-map [x y z]]
                             (assoc height-map [x y] [id (+ z delta)]))
                           height-map
                           points)]
    [height-map supported-by]))

(defn calc-supported-by [bricks]
  (->> bricks
       (map-indexed vector)
       (reduce (fn [state [id brick]]
                 (add-brick state id brick))
               [{} {}])
       second))

(defn part-1 [file]
  (let [bricks (read-input file)]
    (->> bricks
         calc-supported-by
         (mapcat (fn [[_ supporting]]
                   (when (= (count supporting) 1)
                     supporting)))
         distinct
         count
         (- (count bricks)))))

(defn calc-supporting [supported-by]
  (->> supported-by
       (mapcat (fn [[above belows]]
                 (for [below belows]
                   [below above])))
       (reduce (fn [m [below above]]
                 (update m below (fnil conj []) above))
               {})))

(defn calc-chain-reaction [supported-by supporting id]
  (loop [fallen #{}
         falling [id]]
    (let [fallen (into fallen falling)
          falling (->> falling
                       (mapcat supporting)
                       (filter (fn [id]
                                 (when-let [below (seq (supported-by id))]
                                   (every? fallen below))))
                       seq)]
      (if falling
        (recur fallen falling)
        (dec (count fallen))))))

(defn part-2 [file]
  (let [bricks (read-input file)
        supported-by (calc-supported-by bricks)
        supporting (calc-supporting supported-by)]
    (->> (range (count bricks))
         (map #(calc-chain-reaction supported-by supporting %))
         (apply +))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))