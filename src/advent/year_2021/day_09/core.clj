(ns advent.year-2021.day-09.core
  (:require [advent.tools :as tools]
            [clojure.set :as set]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (map-indexed (fn [y line]
                      (map-indexed (fn [x ch]
                                     [[x y] (Integer/parseInt (str ch))])
                                   line)))
       (apply concat)
       (into {})))

(def directions [[0 -1]
                 [0 1]
                 [-1 0]
                 [1 0]])

(defn neighbours [point]
  (mapv #(mapv + point %) directions))

(defn part-1 [file]
  (let [cave-map (read-input file)]
    (->> cave-map
         (filter (fn [[point height]]
                   (->> point
                        neighbours
                        (map cave-map)
                        (filter some?)
                        (every? #(> % height)))))
         (map (comp inc val))
         (apply +))))

(defn find-next-basin [points]
  (loop [baisin #{(first points)}
         search [(first points)]
         counter 20]
    (assert (not= 0 counter))
    (if-let [next-search (->> search
                              (mapcat neighbours)
                              distinct
                              (filter points)
                              (remove baisin)
                              seq)]
      (recur (into baisin next-search)
             next-search
             (dec counter))
      baisin)))

(defn part-2 [file]
  (let [cave-map (read-input file)
        points (->> cave-map
                    (remove (comp #{9} val))
                    (map key)
                    (into #{}))
        baisin-counts (loop [points points
                             baisin-counts []]
                        (if (empty? points)
                          baisin-counts
                          (let [baisin (find-next-basin points)]
                            (recur (set/difference points baisin)
                                   (conj baisin-counts (count baisin))))))]
    (->> baisin-counts
         (sort >)
         (take 3)
         (apply *))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))