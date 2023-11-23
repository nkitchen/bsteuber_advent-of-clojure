(ns advent.year-2019.day-10.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (map-indexed (fn [y line]
                      (->> line
                           (map-indexed (fn [x ch]
                                          (when (= ch \#)
                                            [x y]))))))
       (apply concat)
       (filter some?)
       vec))

(defn rounding [x]
  (Math/round (double (* 100000 x))))

(defn ->quadrep [[x y]]
  ;; First number is the quadrant
  ;; Second number the steepness ordered clockwise
  (cond
    (and (neg? y) (>= x 0))
    [0 (rounding (- (/ x y)))]

    (and (pos? x) (>= y 0))
    [1 (rounding (/ y x))]

    (and (pos? y) (<= x 0))
    [2 (rounding (- (/ x y)))]

    (and (neg? x) (<= y 0))
    [3 (rounding (/ y x))]))

(defn len [[x y]]
  (+ (* x x) (* y y)))

(comment
  (let [ps [[0 -1]
            [1 -1]
            [2 -1]
            [1 0]
            [1 1]
            [1 2]
            [0 1]
            [-1 1]
            [-2 1]
            [-1 0]
            [-1 -1]
            [-1 -2]]]
    (mapv ->quadrep ps)))


(defn find-best-station [asteroids]
  (->> asteroids
       (map (fn [start]
              [start
               (->> asteroids
                    (map (fn [end]
                           (when-not (= start end)
                             (let [delta (mapv - start end)]
                               (->quadrep delta)))))
                    (filter some?)
                    distinct
                    count)]))
       (apply max-key second)))

(defn part-1 [file]
  (let [asteroids (read-input file)]
    (second (find-best-station asteroids))))

(defn next-asteroid [[_ by-quadrep]]
  (when-let [[quadrep [asteroid & more]] (first by-quadrep)]
    (let [next-asteroids (seq more)
          next-by-quadrep (concat (next by-quadrep)
                                  (when next-asteroids
                                    [[quadrep next-asteroids]]))]
      [asteroid next-by-quadrep])))

(defn part-2 [file]
  (let [asteroids (read-input file)
        [station _] (find-best-station asteroids)
        rel-asteroids (->> asteroids
                           (remove #{station})
                           (mapv #(mapv - % station)))
        by-quadrep (->> rel-asteroids
                        (group-by ->quadrep)
                        sort
                        (mapv (fn [[quadrep asteroids]]
                                [quadrep (sort-by len asteroids)])))
        rel-asteroid (nth
                      (->> [nil by-quadrep]
                           (iterate next-asteroid)
                           next
                           (map first)
                           (take-while some?))
                      199)
        [x y] (mapv + station rel-asteroid)]
    (+ (* x 100) y)))

(comment
  (assert
   (= 8 (part-1 "test")))
  (assert
   (= 33 (part-1 "test2")))
  (part-1 "input")
  (assert
   (= 802 (part-2 "test3")))
  (part-2 "input"))