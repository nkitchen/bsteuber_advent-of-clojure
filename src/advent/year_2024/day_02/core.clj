(ns advent.year-2024.day-02.core
  (:require
   [advent.tools :as tools]
   [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (->> (str/split line #" ")
                    (mapv tools/read-int))))))

(defn is-safe-part-1? [report]
  (let [safe-steps (->> report
                        (partition 2 1)
                        (map (fn [[x y]]
                               (let [diff (- y x)]
                                 (cond (<= -3 diff -1) :dec
                                       (<= 1 diff 3)   :inc)))))]
    (when-let [first-step (first safe-steps)]
      (every? #(= first-step %) (rest safe-steps)))))

(defn part-1 [file]
  (->> file
       read-input
       (filter is-safe-part-1?)
       count))

(defn is-safe-part-2? [report]
  (->> (range (count report))
       (map (fn [omitted-level]
              (let [[before starting-at] (split-at omitted-level report)]
                (concat before (rest starting-at)))))
       (some is-safe-part-1?)))

(defn part-2 [file]
  (->> file
       read-input
       (filter is-safe-part-2?)
       count))

(comment
  (read-input "test")
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
