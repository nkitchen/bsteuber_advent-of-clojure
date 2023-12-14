(ns advent.year-2023.day-11.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  (->> (tools/read-grid file (fn [ch]
                               (= ch \#)))
       keys
       (into #{})))

(defn calc-empty-lines [elt-fn galaxies]
  (let [coords (->> galaxies
                    (map elt-fn)
                    distinct
                    (into #{}))
        min-val (apply min coords)
        max-val (apply max coords)]
    (->> (range (inc min-val) max-val)
         (remove coords)
         (into #{}))))

(def calc-empty-cols (partial calc-empty-lines first))

(def calc-empty-rows (partial calc-empty-lines second))


(defn solve [file expanse-factor]
  (let [galaxies (read-input file)
        empty-cols (calc-empty-cols galaxies)
        empty-rows (calc-empty-rows galaxies)
        galaxy-vec (vec galaxies)
        num-galaxies (count galaxy-vec)]
    (->> (for [index-1 (range (dec num-galaxies))
               index-2 (range (inc index-1) num-galaxies)]
           [index-1 index-2])
         (map (fn [[index-1 index-2]]
                (let [[x1 y1] (galaxy-vec index-1)
                      [x2 y2] (galaxy-vec index-2)
                      min-x (min x1 x2)
                      max-x (max x1 x2)
                      min-y (min y1 y2)
                      max-y (max y1 y2)
                      dx (- max-x min-x)
                      dy (- max-y min-y)
                      empty-col-count (->> (range (inc min-x) max-x)
                                           (filter empty-cols)
                                           count)
                      empty-row-count (->> (range (inc min-y) max-y)
                                           (filter empty-rows)
                                           count)]
                  (+ dx dy (* (+ empty-col-count empty-row-count)
                              (dec expanse-factor))))))
         (apply +))))

(comment
  (solve "test" 2)
  (solve "input" 2)
  (solve "test" 10)
  (solve "test" 100)
  (solve "input" 1000000))