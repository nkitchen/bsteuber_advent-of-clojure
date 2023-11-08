(ns advent.year-2021.day-22.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-var-range [s]
  (let [[_ start-end] (str/split s #"=")]
    (->> (str/split start-end #"\.\.")
         (mapv tools/read-int))))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (let [[cmd ranges] (str/split line #" ")
                     cmd (keyword cmd)
                     cuboid (->> (str/split ranges #",")
                                 (mapv read-var-range))]
                 {:cmd cmd :cuboid cuboid})))))

(defn valid-interval? [[start end]]
  (<= start end))

(defn valid-cuboid? [cuboid]
  (every? valid-interval? cuboid))

(defn interval-intersection [[start-1 end-1] [start-2 end-2]]
  (when (and (<= start-2 end-1)
             (<= start-1 end-2))
    [(max start-1 start-2) (min end-1 end-2)]))

(defn cuboid-intersection [cuboid-1 cuboid-2]
  (let [res (mapv interval-intersection cuboid-1 cuboid-2)]
    (when (every? some? res)
      res)))

(defn cuboid-difference [cuboid substract-cuboid]
  (if-let [[[int-x-start int-x-end]
            [int-y-start int-y-end]
            [int-z-start int-z-end] :as inters]
           (cuboid-intersection substract-cuboid cuboid)]
    (let [[[x-start x-end]
           [y-start y-end]
           [z-start z-end]] cuboid]
      (->> (for [xint [[x-start (dec int-x-start)]
                       [int-x-start int-x-end]
                       [(inc int-x-end) x-end]]
                 yint [[y-start (dec int-y-start)]
                       [int-y-start int-y-end]
                       [(inc int-y-end) y-end]]
                 zint [[z-start (dec int-z-start)]
                       [int-z-start int-z-end]
                       [(inc int-z-end) z-end]]
                 :when (not= [xint yint zint] inters)]
             [xint yint zint])
           (filter valid-cuboid?)))
    [cuboid]))

(defn turn-on [cuboids on-cuboid]
  (let [on-cuboids
        (reduce (fn [on-cuboids existing-cuboid]
                  (mapcat (fn [on-cuboid]
                            (cuboid-difference on-cuboid existing-cuboid))
                          on-cuboids))
                [on-cuboid]
                cuboids)]
    (into cuboids on-cuboids)))

(defn turn-off [cuboids off-cuboid]
  (mapcat (fn [cuboid]
            (cuboid-difference cuboid off-cuboid))
          cuboids))

(defn apply-step [cuboids {:keys [cmd cuboid]}]
  (case cmd
    :on (turn-on cuboids cuboid)
    :off (turn-off cuboids cuboid)))

(defn restrict-part-1 [cuboid]
  (->> cuboid
       (mapv (fn [[start end]]
               [(max start -50)
                (min end 50)]))))

(defn count-cubes [cuboids]
  (->> cuboids
       (map (fn [cuboid]
              (->> cuboid
                   (map (fn [[start end]]
                          (assert (<= start end))
                          (inc (- end start))))
                   (apply *))))
       (apply +)))

(defn part-1 [file]
  (->> file
       read-input
       (map #(update % :cuboid restrict-part-1))
       (filter #(valid-cuboid? (:cuboid %)))
       (reduce apply-step [])
       count-cubes))

(defn part-2 [file]
  (->> file
       read-input
       (reduce apply-step [])
       count-cubes))

(comment
  (part-1 "test")
  (part-1 "test-2")
  (part-1 "input")
  (part-2 "test-3")
  (part-2 "input"))