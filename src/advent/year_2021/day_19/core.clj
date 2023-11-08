(ns advent.year-2021.day-19.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-blocks
       (mapv (fn [lines]
               (->> lines
                    next
                    (mapv (fn [line]
                            (->> (str/split line #",")
                                 (mapv tools/read-int)))))))))

(defn beacon-orientations [[x y z]]
  (->> [[x y z]
        [(- x) (- y) z]
        [x (- y) (- z)]]
       (mapcat (fn [[x y z]]
                 [[x y z]
                  [y z x]
                  [z x y]]))
       (mapcat (fn [[x y z]]
                 [[x y z]
                  [x (- z) y]
                  [x (- y) (- z)]
                  [x z (- y)]]))
       distinct))

(defn scanner-orientations [beacons]
  (->> beacons
       (map beacon-orientations)
       (apply map vector)))

(defn matching-orientation [fixed-beacons loose-beacons]
  (->> loose-beacons
       scanner-orientations
       (some (fn [loose-beacons]
               (when-let [scanner-position (->> (for [x fixed-beacons
                                                      y loose-beacons]
                                                  (mapv - x y))
                                                frequencies
                                                (some (fn [[distance freq]]
                                                        (when (>= freq 12)
                                                          distance))))]
                 [scanner-position
                  (mapv (fn [beacon]
                          (mapv + beacon scanner-position))
                        loose-beacons)])))))
(defn solve [file]
  (let [scanners (read-input file)]
    (loop [loose-scanners (->> scanners
                               (map-indexed vector)
                               next
                               (into {}))
           fixed-scanners {0 [[0 0 0] (first scanners)]}
           todo [(first scanners)]]
      (if-let [fixed-scanner (first todo)]
        (let [indexed-matches (->> loose-scanners
                                   (map (fn [[id loose-scanner]]
                                          (when-let [match (matching-orientation fixed-scanner loose-scanner)]
                                            [id match])))
                                   (filter some?))
              next-loose-scanners (->> indexed-matches
                                       (map first)
                                       (apply dissoc loose-scanners))
              next-fixed-scanners (into fixed-scanners indexed-matches)
              next-todo (concat (next todo) (map (comp second second) indexed-matches))]
          (recur next-loose-scanners next-fixed-scanners next-todo))
        {:beacons (->> fixed-scanners
                       vals
                       (map second)
                       (apply concat)
                       distinct
                       vec)
         :scanners (->> fixed-scanners
                        vals
                        (mapv first))}))))

(defn part-1 [file]
  (count (:beacons (solve file))))

(defn part-2 [file]
  (let [scanners (:scanners (solve file))]
    (->> (for [x scanners
               y scanners]
           (->> (map - x y)
                (map #(Math/abs %))
                (apply +)))
         (apply max))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))