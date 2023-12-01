(ns advent.year-2019.day-20.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  (let [raw-grid (tools/read-grid
                  file
                  (fn [ch]
                    (cond
                      (= ch \.) :empty
                      (<= (int \A)
                          (int ch)
                          (int \Z)) ch)))
        points (->> raw-grid
                    (filter (comp #{:empty} val))
                    (map first)
                    (into #{}))
        letter? (fn [ch]
                  (and ch
                       (not= :empty ch)))
        labels (->> raw-grid
                    (map (fn [[[x y] ch]]
                           (when (letter? ch)
                             (let [left [(dec x) y]
                                   at-left (raw-grid left)
                                   top [x (dec y)]
                                   at-top (raw-grid top)]
                               (when-not (or (letter? at-left)
                                             (letter? at-top))
                                 (let [at-right (raw-grid [(inc x) y])
                                       at-bot (raw-grid [x (inc y)])
                                       label (str ch
                                                  (if (letter? at-right)
                                                    at-right
                                                    at-bot))
                                       point (some
                                              (fn [p]
                                                (when (= :empty (raw-grid p))
                                                  p))
                                              [left
                                               top
                                               [(+ x 2) y]
                                               [x (+ y 2)]])]
                                   [label point]))))))
                    (filter some?)
                    (group-by first)
                    (map (fn [[label labelled-points]]
                           [label (mapv second labelled-points)]))
                    (into {}))
        start (first (labels "AA"))
        end (first (labels "ZZ"))
        portals (->> labels
                     (mapcat (fn [[_ [point-1 point-2]]]
                               (when (and point-1 point-2)
                                 [[point-1 point-2]
                                  [point-2 point-1]])))
                     (into {}))]
    {:points points
     :start start
     :end end
     :portals portals}))

(defn neighbours [{:keys [points portals]} [x y]]
  (let [direct-neighbours (->> [[(inc x) y]
                                [(dec x) y]
                                [x (inc y)]
                                [x (dec y)]]
                               (filter points))
        portal-neighbours (when-let [neighb (portals [x y])]
                            [neighb])]
    (concat direct-neighbours portal-neighbours)))

(defn part-1 [file]
  (let [{:keys [start end]
         :as data} (read-input file)]
    (loop [seen #{}
           explore [start]
           steps 0]
      (if (some #{end} explore)
        steps
        (let [seen (into seen explore)
              explore (->> explore
                           (mapcat #(neighbours data %))
                           distinct)]
          (recur seen explore (inc steps)))))))

(comment
  (part-1 "test")
  (part-1 "test2")
  (part-1 "input"))