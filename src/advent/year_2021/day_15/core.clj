(ns advent.year-2021.day-15.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  (let [lines (->> file
                   tools/read-lines)
        risks (->> lines
                   (map-indexed (fn [y line]
                                  (->> line
                                       (map-indexed (fn [x ch]
                                                      [[x y] (tools/read-int ch)])))))
                   (apply concat)
                   (into {}))]
    {:risks risks
     :size (count lines)}))

(def directions [[0 -1] [0 1] [-1 0] [1 0]])

(defn neighbours [point]
  (map #(mapv + point %) directions))

(defn lowest-risk-path [{:keys [risks size]}]
  (let [goal [(dec size)
              (dec size)]]
    (loop [lowest-risk-from {goal 0}
           changed #{goal}]
      (if (empty? changed)
        (lowest-risk-from [0 0])
        (let [[lowest-risk-from changed]
              (->> changed
                   (mapcat (fn [point]
                             (let [next-risk (+ (lowest-risk-from point)
                                                (risks point))]
                               (->> point
                                    neighbours
                                    (filter risks)
                                    (map (fn [neighb]
                                           [neighb next-risk]))))))
                   (reduce (fn [[lowest-risk-from changed] [point risk]]
                             (if (< risk (lowest-risk-from point Integer/MAX_VALUE))
                               [(assoc lowest-risk-from point risk)
                                (conj changed point)]
                               [lowest-risk-from changed]))
                           [lowest-risk-from #{}]))]
          (recur lowest-risk-from changed))))))

(defn part-1 [file]
  (lowest-risk-path (read-input file)))

(defn part-2 [file]
  (let [{:keys [risks size]} (read-input file)
        risks (->> (for [tile-x (range 5)
                         tile-y (range 5)
                         inner-x (range size)
                         inner-y (range size)]
                     (let [x (+ (* tile-x size) inner-x)
                           y (+ (* tile-y size) inner-y)
                           value (-> (+ (risks [inner-x inner-y])
                                        tile-x
                                        tile-y)
                                     dec
                                     (mod 9)
                                     inc)]
                       [[x y] value]))
                   (into {}))
        size (* 5 size)]
    (lowest-risk-path {:risks risks
                       :size size})))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))