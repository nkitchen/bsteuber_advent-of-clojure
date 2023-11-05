(ns advent.year-2021.day-11.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  {:octopi (->> file
                tools/read-lines
                (map-indexed (fn [y line]
                               (map-indexed (fn [x ch]
                                              [[x y] (tools/read-int ch)])
                                            line)))
                (apply concat)
                (into {}))
   :flashes 0})

(def directions (for [x [-1 0 1]
                      y [-1 0 1]
                      :when (or (not= x 0)
                                (not= y 0))]
                  [x y]))

(def all-points
  (->> (for [x (range 10)
             y (range 10)]
         [x y])
       (into #{})))

(defn neighbours [point]
  (->> directions
       (map #(mapv + % point))
       (filter all-points)))

(defn next-state [{:keys [octopi flashes]}]
  (let [octopi (update-vals octopi inc)]
    (loop [octopi octopi
           flashed #{}
           flashing (->> octopi
                         (filter (fn [[_ value]]
                                   (> value 9)))
                         (map key))]
      (if-let [first-flashing (first flashing)]
        (if (flashed first-flashing)
          (recur octopi
                 flashed
                 (next flashing))
          (let [neighbs (neighbours first-flashing)
                octopi (->> neighbs
                            (reduce (fn [octopi neighb]
                                      (update octopi neighb inc))
                                    octopi))
                also-flashing (->> neighbs
                                   (filter (fn [neighb]
                                             (> (octopi neighb) 9))))]
            (recur octopi
                   (conj flashed first-flashing)
                   (concat (next flashing) also-flashing))))
        {:octopi (reduce (fn [octopi point]
                           (assoc octopi point 0))
                         octopi
                         flashed)
         :flashes (+ flashes (count flashed))}))))

(defn print-state [{:keys [octopi flashes]}]
  (println "\n\nflashes:" flashes)
  (doseq [y (range 10)]
    (doseq [x (range 10)]
      (print (octopi [x y])))
    (println)))

(defn part-1 [file]
  (->> file
       read-input
       (iterate next-state)
       (drop 100)
       first
       :flashes))

(defn part-2 [file]
  (->> file
       read-input
       (iterate next-state)
       (map :flashes)
       (partition 2 1)
       (map-indexed (fn [i [flashes-before flashes-after]]
                      (when (= (- flashes-after flashes-before)
                               100)
                        (inc i))))
       (some identity)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))