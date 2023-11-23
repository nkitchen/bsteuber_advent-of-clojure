(ns advent.year-2019.day-03.core
  (:require [clojure.string :as str]
            [advent.tools :as tools]))

(defn read-wire [line]
  (->> (str/split line #",")
       (mapv (fn [token]
               (let [direction (case (first token)
                                 \R :right
                                 \L :left
                                 \U :up
                                 \D :down)
                     number (tools/read-int (subs token 1))]
                 [direction number])))))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv read-wire)))

(defn calc-next-pos [[x y] [dir number]]
  (case dir
    :up [x (- y number)]
    :down [x (+ y number)]
    :left [(- x number) y]
    :right [(+ x number) y]))

(defn calc-lines [wire]
  (->> (reductions calc-next-pos [0 0] wire)
       (partition 2 1)))

(defn vertical-lines [lines]
  (->> lines
       (filter (fn [[[x1 _] [x2 _]]]
                 (= x1 x2)))
       (map (comp vec sort))))

(defn horizontal-lines [lines]
  (->> lines
       (filter (fn [[[_ y1] [_ y2]]]
                 (= y1 y2)))
       (map (comp vec sort))))

(defn intersection [horizontal vertical]
  (let [[[hor-x1 hor-y] [hor-x2 _]] horizontal
        [[ver-x ver-y1] [_ ver-y2]] vertical]
    (when (and (<= hor-x1 ver-x hor-x2)
               (<= ver-y1 hor-y ver-y2))
      [ver-x hor-y])))

(defn all-intersections [file]
  (let [[wire-1 wire-2] (->> file
                             read-input
                             (map calc-lines)
                             (map next))
        hor-1 (horizontal-lines wire-1)
        hor-2 (horizontal-lines wire-2)
        ver-1 (vertical-lines wire-1)
        ver-2 (vertical-lines wire-2)]
    (->> (concat
          (for [hor hor-1
                ver ver-2]
            (intersection hor ver))
          (for [hor hor-2
                ver ver-1]
            (intersection hor ver)))
         (filter some?))))

(defn manhattan-distance
  ([p]
   (manhattan-distance p [0 0]))
  ([[x1 y1] [x2 y2]]
   (+ (Math/abs (- x1 x2))
      (Math/abs (- y1 y2)))))

(defn part-1 [file]
  (->> (all-intersections file)
       (map manhattan-distance)
       (apply min)))

(defn steps-to-intersection [lines [x y]]
  (loop [steps 0
         lines lines]
    (if-let [[[x1 y1] [x2 y2]] (first lines)]
      (if (and (or (<= x1 x x2)
                   (<= x2 x x1))
               (or (<= y1 y y2)
                   (<= y2 y y1)))
        (+ steps (manhattan-distance [x1 y1] [x y]))
        (recur (+ steps (manhattan-distance [x1 y1] [x2 y2]))
               (next lines)))
      (throw (ex-info "Intersection not found" {})))))

(defn sum-of-steps [[wire-1 wire-2] intersection]
  (+ (steps-to-intersection wire-1 intersection)
     (steps-to-intersection wire-2 intersection)))

(defn part-2 [file]
  (let [wires (map calc-lines (read-input file))]
    (->> (all-intersections file)
         (map (partial sum-of-steps wires))
         (apply min))))

(comment
  (part-1 "test3")
  (part-1 "test")
  (part-1 "test2")
  (part-1 "input")
  (part-2 "test3")
  (part-2 "input"))