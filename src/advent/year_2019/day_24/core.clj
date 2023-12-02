(ns advent.year-2019.day-24.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  (->>
   (tools/read-grid file (fn [ch]
                           (when (= ch \#)
                             true)))
   keys
   (into #{})))

(defn add-layer [layer points]
  (map #(conj % layer) points))

(defn read-input-part-2 [file]
  (->> file
       (read-input)
       (add-layer 0)
       (into #{})))

(defn neighbours [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn neighbours-part-2 [[x y layer]]
  (let [this-layer (neighbours [x y])
        outer-layer (->> [(when (= x 0)
                            [1 2])
                          (when (= x 4)
                            [3 2])
                          (when (= y 0)
                            [2 1])
                          (when (= y 4)
                            [2 3])]
                         (filter some?))
        inner-layer (case [x y]
                      [1 2] (for [y (range 5)] [0 y])
                      [2 1] (for [x (range 5)] [x 0])
                      [3 2] (for [y (range 5)] [4 y])
                      [2 3] (for [x (range 5)] [x 4])
                      nil)]
    (concat
     (add-layer layer this-layer)
     (add-layer (dec layer) outer-layer)
     (add-layer (inc layer) inner-layer))))

(defn next-infested? [state neighb-fn point]
  (let [neighb-count (->> point
                          neighb-fn
                          (map state)
                          (filter some?)
                          count)]
    (if (state point)
      (= neighb-count 1)
      (<= 1 neighb-count 2))))

(defn next-state [state]
  (->> (for [x (range 5)
             y (range 5)]
         [x y])
       (filter (partial next-infested? state neighbours))
       (into #{})))

(defn next-state-part-2 [state]
  (let [min-layer (->> state (map peek) (apply min))
        max-layer (->> state (map peek) (apply max))]
    (->> (for [x (range 5)
               y (range 5)
               :when (not (= x y 2))
               layer (range (dec min-layer)
                            (+ max-layer 2))]
           [x y layer])
         (filter (partial next-infested? state neighbours-part-2))
         (into #{}))))

(defn part-1 [file]
  (loop [state (read-input file)
         seen #{}]
    (if (seen state)
      (->> state
           (map (fn [[x y]]
                  (let [i (+ x (* y 5))]
                    (int (Math/pow 2 i)))))
           (apply +))
      (recur (next-state state) (conj seen state)))))

(defn part-2 [file steps]
  (->> file
       read-input-part-2
       (iterate next-state-part-2)
       (drop steps)
       first
       count))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test" 10)
  (part-2 "input" 200))