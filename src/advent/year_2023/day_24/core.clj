(ns advent.year-2023.day-24.core
  (:require
   [advent.tools :as tools]
   [clojure.math :as math]
   [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (->> (str/split line #" @ +")
                    (mapv (fn [nums]
                            (->> (str/split nums #", +")
                                 (mapv tools/read-long)))))))))


(defn to-normal-form [[[x y] [vx vy]]]
  (let [m (/ vy vx)
        b (- y (* x m))]
    [m b]))


(defn calc-intersection [hail-1 hail-2]
  (let [[m1 b1] (to-normal-form hail-1)
        [m2 b2] (to-normal-form hail-2)]
    (when-not (= m1 m2)
      ;; m1 x + b1 = m2 x + b2
      ;; x = (b2 - b1) / (m1 - m2)
      (let [x (/ (- b2 b1)
                 (- m1 m2))
            y (+ (* m1 x) b1)]
        [x y]))))

(defn part-1 [file [test-area-min test-area-max]]
  (let [hail-stones (read-input file)]
    (->> (for [i1 (range (count hail-stones))
               i2 (range (inc i1) (count hail-stones))]
           (let [hail-stone-1 (hail-stones i1)
                 hail-stone-2 (hail-stones i2)
                 [[x1] [vx1]] hail-stone-1
                 [[x2] [vx2]] hail-stone-2]
             (when-let [[x y] (calc-intersection hail-stone-1 hail-stone-2)]
               (and (<= test-area-min x test-area-max)
                    (<= test-area-min y test-area-max)
                    (= (math/signum vx1)
                       (math/signum (- x x1)))
                    (= (math/signum vx2)
                       (math/signum (- x x2)))))))
         (filter identity)
         count)))

;; x + t*vx = x1 + t*vx1
;; t = (x1 - x) / (vx - vx1) = (y1 - y) / (vy - vy1) = (z1 - z) / (vz - vz1)
;; (x1 - x) * (vy - vy1) = (y1 - y) * (vx - vx1)
;; t > 0, t is integer
;; y + t*vy = y1 + t*vy1
;; z + t*vz = z1 + t*vz1


(comment
  (doseq [stone (read-input "input")]
    (prn stone))
  (part-1 "test" [7 27])
  (part-1 "input" [200000000000000 400000000000000]))
