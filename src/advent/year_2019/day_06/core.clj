(ns advent.year-2019.day-06.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (map (fn [line]
              (->> (str/split line #"\)")
                   reverse
                   vec)))
       (into {})))

(defn count-all-orbits [m]
  (let [cache (atom {})
        rec-count (fn rec-count [o]
                    (if-let [res (@cache o)]
                      res
                      (if-let [parent (m o)]
                        (inc (rec-count parent))
                        0)))]
    (->> m
         keys
         (map rec-count)
         (apply +))))

(defn part-1 [file]
  (count-all-orbits (read-input file)))

(defn parents-with-steps [m o]
  (->> (iterate (fn [[o steps]]
                  (when-let [p (m o)]
                    [p (inc steps)]))
                [(m o) 0])
       (take-while some?)))

(defn part-2 [file]
  (let [m (read-input file)
        you-parents (parents-with-steps m "YOU")
        san-parents-m (into {} (parents-with-steps m "SAN"))]
    (some (fn [[parent you-steps]]
            (when-let [san-steps (san-parents-m parent)]
              (+ you-steps san-steps)))
          you-parents)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test2")
  (part-2 "input"))