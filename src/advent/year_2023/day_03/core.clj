(ns advent.year-2023.day-03.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  (tools/read-grid file (fn [ch]
                          (cond
                            (tools/digit? ch) [:digit ch]
                            (not= ch \.)  [:symbol ch]))))

(defn neighbours [[px py]]
  (for [x [(dec px) px (inc px)]
        y [(dec py) py (inc py)]
        :when (not= [x y] [px py])]
    [x y]))

(defn iter-x [f grid [x y]]
  (->> (iterate f x)
       (map (fn [x]
              [x y]))
       (take-while (fn [point]
                     (= :digit (first (grid point)))))))

(defn number-start [grid point]
  (last (iter-x dec grid point)))

(defn read-number [grid point]
  (->> (iter-x inc grid point)
       (map (comp second grid))
       (apply str)
       tools/read-int))

(defn part-1 [file]
  (let [grid (read-input file)]
    (->> grid
         (filter (fn [[_ [type _]]]
                   (= :symbol type)))
         (map key)
         (mapcat neighbours)
         distinct
         (filter (fn [point]
                   (when-let [[type _] (grid point)]
                     (= type :digit))))
         (map (partial number-start grid))
         distinct
         (map (partial read-number grid))
         (apply +))))

(defn part-2 [file]
  (let [grid (read-input file)]
    (->> grid
         (filter (fn [[_ [_ ch]]]
                   (= ch \*)))
         (map key)
         (map (fn [point]
                (let [number-starts (->> point
                                         neighbours
                                         (filter (fn [point]
                                                   (when-let [[type _] (grid point)]
                                                     (= type :digit))))
                                         (map (partial number-start grid))
                                         distinct)]
                  (when (= (count number-starts) 2)
                    (->> number-starts
                         (map (partial read-number grid))
                         (apply *))))))
         (filter some?)
         (apply +))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))