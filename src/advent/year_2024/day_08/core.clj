(ns advent.year-2024.day-08.core
  (:require
   [advent.tools :as tools]
   [advent.grid :as grid]))

(defn read-input [file]
  (assoc (tools/read-grid-dimensions file)
         :antennas (->> (tools/read-grid file (fn [ch]
                                                (when-not (= ch \.)
                                                  ch)))
                        tools/value-multimap)))

(defn antinodes-1 [[pos-1 pos-2]]
  (let [diff (grid/minus pos-2 pos-1)]
    [(grid/plus pos-2 diff)
     (grid/minus pos-1 diff)]))

(defn solve [antinodes-fn {:keys [antennas]
                           :as dimensions}]
  (->> antennas
       (mapcat (fn [[_ positions]]
                 (->> (tools/all-pairs positions)
                      (mapcat antinodes-fn))))
       (filter (partial grid/on-grid? dimensions))
       distinct
       count))

(defn part-1 [file]
  (let [data (read-input file)]
    (solve antinodes-1 data)))

(defn antinodes-2 [dimensions [pos-1 pos-2]]
  (let [diff (grid/minus pos-2 pos-1)
        on-grid? (partial grid/on-grid? dimensions)]
    (concat
     (->> (iterate #(grid/plus % diff)
                   pos-2)
          (take-while on-grid?))
     (->> (iterate #(grid/minus % diff)
                   pos-1)
          (take-while on-grid?)))))

(defn part-2 [file]
  (let [data (read-input file)]
    (solve (partial antinodes-2 data) data)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
