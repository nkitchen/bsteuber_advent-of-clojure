(ns advent.year-2023.day-05.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-numbers [s]
  (->> (str/split s #" ")
       (mapv tools/read-long)))

(defn read-input [file]
  (let [[[seeds-line] &  map-blocks] (tools/read-blocks file)
        [_ seeds] (str/split seeds-line #": ")]
    {:seeds (read-numbers seeds)
     :mappings (->> map-blocks
                    (mapv (fn [lines]
                            (->> lines
                                 next
                                 (mapv read-numbers)))))}))

(defn interval? [[start end]]
  (<= start end))

(defn interval-intersection [[x-start x-end] [y-start y-end]]
  (let [res [(max x-start y-start) (min x-end y-end)]]
    (when (interval? res)
      res)))

(defn apply-range [[seed-min seed-max :as seed-range] [dest-start src-start range-len]]
  (let [src-end (+ src-start range-len -1)
        match (interval-intersection
               seed-range
               [src-start src-end])]
    (if match
      (let [[match-start match-end] match
            delta (- dest-start src-start)
            replaced [(+ delta match-start)
                      (+ delta match-end)]
            before [seed-min (dec match-start)]
            after [(inc match-end) seed-max]]
        [[replaced] (filterv interval? [before after])])
      [[] [seed-range]])))

(defn apply-mapping [seed-ranges mapping-ranges]
  (let [[matched
         todo] (reduce
                (fn [[matched todo] map-range]
                  (let [results (map #(apply-range % map-range) todo)
                        matched (->> results
                                     (mapcat first)
                                     (into matched))
                        todo (mapcat second results)]
                    [matched todo]))
                [[] seed-ranges]
                mapping-ranges)]
    (concat matched todo)))

(defn apply-all [seed-ranges mappings]
  (reduce apply-mapping seed-ranges mappings))

(defn solve [part-2? file]
  (let [{:keys [seeds mappings]} (read-input file)
        seed-ranges (if part-2?
                      (->> seeds
                           (partition 2)
                           (map (fn [[start length]]
                                  [start (+ start length -1)])))
                      (->> seeds
                           (map (fn [seed]
                                  [seed seed]))))
        location-ranges (apply-all seed-ranges mappings)]
    (->> location-ranges
         (map first)
         (apply min))))

(def part-1 (partial solve false))

(def part-2 (partial solve true))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))