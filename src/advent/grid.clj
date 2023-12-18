(ns advent.grid)

(def left [-1 0])
(def right [1 0])
(def up [0 -1])
(def down [0 1])

(def directions [up right down left])

(def opposite-dir
  {up down
   down up
   left right
   right left})

(def rotate-left
  {up left
   left down
   down right
   right up})

(def rotate-right
  {up right
   left up
   down left
   right down})

(defn go-dir
  ([point direction]
   (go-dir point direction 1))
  ([point direction steps]
   (->> direction
        (map (partial * steps))
        (mapv + point))))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))