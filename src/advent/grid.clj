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

(def go-dir (partial mapv +))