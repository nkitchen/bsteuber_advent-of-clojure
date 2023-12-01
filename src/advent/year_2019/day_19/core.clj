(ns advent.year-2019.day-19.core
  (:require [advent.year-2019.intcode :as intcode]))

(def grid-size 50)

(def program (intcode/read-input-program))

(defn point-in-beam? [point]
  (->> point
       (intcode/run-until-halt program)
       :output
       first
       (= 1)))

(defn detect-grid []
  (->> (for [x (range grid-size)
             y (range grid-size)]
         [x y])
       (filter point-in-beam?)
       (into #{})))

(defn print-grid [grid]
  (doseq [y (range grid-size)]
    (doseq [x (range grid-size)]
      (print (if (grid [x y])
               "#"
               ".")))
    (println)))

(defn part-1 []
  (count (detect-grid)))

(defn next-edge-point [[x y]]
  (->> [[x (inc y)]
        [(inc x) (inc y)]
        [(inc x) y]]
       (some (fn [point]
               (when (point-in-beam? point)
                 point)))))

(defn part-2 []
  (->> (iterate next-edge-point [8 9])
       (take 100000)
       (some (fn [[lx ly]]
               (let [rx (+ lx 99)
                     ry (- ly 99)]
                 (when (point-in-beam? [rx ry])
                   (+ (* lx 10000)
                      ry)))))))

(comment
  (print-grid (detect-grid))
  (part-1)
  (part-2))
