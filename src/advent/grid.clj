(ns advent.grid)

(defn go-dir
  ([point direction]
   (go-dir point direction 1))
  ([point direction steps]
   (->> direction
        (map (partial * steps))
        (mapv + point))))

(def west [-1 0])
(def east [1 0])
(def north [0 -1])
(def south [0 1])
(def north-west (go-dir north west))
(def north-east (go-dir north east))
(def south-west (go-dir south west))
(def south-east (go-dir south east))


(def directions-4 [north east south west])
(def diagonals [north-west north-east south-west south-east])
(def directions-8 (into directions-4 diagonals))

(def opposite-dir
  {north south
   south north
   west east
   east west})

(def rotate-left
  {north west
   west south
   south east
   east north})

(def rotate-right
  {north east
   west north
   south west
   east south})

(defn neighbors-4 [point]
  (->> directions-4
       (map #(go-dir point %))))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))

(defn all-points [{:keys [rows cols]}]
  (for [x (range rows)
        y (range cols)]
    [x y]))
