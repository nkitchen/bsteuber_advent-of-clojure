(ns advent.year-2024.day-18.core
  (:require
   [advent.tools :as tools]
   [clojure.string :as str]
   [advent.grid :as grid]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (->> (str/split line #",")
                    (mapv tools/read-int))))))

(defn solve [grid max-coord]
  (let [allowed? (fn [[x y :as point]]
                   (and (not (grid point))
                        (<= 0 x max-coord)
                        (<= 0 y max-coord)))]
    (loop [explore [[0 0]]
           seen #{}
           steps 0]
      (let [seen (into seen explore)]
        (cond
          (empty? explore)
          nil

          (seen [max-coord max-coord])
          steps

          :else
          (let [explore (->> explore
                             (mapcat grid/neighbors-4)
                             (filter allowed?)
                             (remove seen)
                             distinct)]
            (recur explore seen (inc steps))))))))

(defn part-1 [file max-coord take-bytes]
  (let [grid (->> (read-input file)
                  (take take-bytes)
                  (into #{}))]
    (solve grid max-coord)))

(defn part-2 [file max-coord]
  (let [bytes (read-input file)]
    (->> (range (count bytes))
         (some (fn [take-bytes]
                 (let [grid (->> bytes
                                 (take take-bytes)
                                 (into #{}))]
                   (when-not (solve grid max-coord)
                     (bytes (dec take-bytes)))))))))

(comment
  (part-1 "test" 6 12)
  (time (part-1 "input" 70 1024))
  (part-2 "test" 6)
  (time (part-2 "input" 70))  ; takes ~40s
  )
