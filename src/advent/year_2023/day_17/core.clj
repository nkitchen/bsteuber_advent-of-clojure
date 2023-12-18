(ns advent.year-2023.day-17.core
  (:require [advent.tools :as tools]
            [clojure.data.priority-map :refer [priority-map-keyfn]]
            [advent.grid :as grid]))

(defn read-input [file]
  (let [{:keys [rows cols]} (tools/read-grid-dimensions file)]
    {:start [0 0]
     :end [(dec cols) (dec rows)]
     :grid (tools/read-grid file tools/read-int)}))

(defn next-states-with-cost [grid stream-fn [point dir]]
  (->> (iterate (fn [[point cost]]
                  (let [point (grid/go-dir point dir)
                        added-cost (grid point)]
                    (when added-cost
                      [point (+ cost added-cost)])))
                [point 0])
       next
       (take-while some?)
       stream-fn
       (mapcat (fn [[point cost]]
                 [[[point (grid/rotate-left dir)] cost]
                  [[point (grid/rotate-right dir)] cost]]))))

(defn solve [{:keys [start end grid]} stream-fn]
  (loop [open-list (priority-map-keyfn
                    first
                    [start grid/right] [0 0]
                    [start grid/down] [0 0])
         closed-list #{}]
    (when-let [[current-node [_ total-cost]] (first open-list)]
      (if (= (first current-node) end)
        total-cost
        (let [open-list (dissoc open-list current-node)
              closed-list (conj closed-list current-node)
              open-list (->> current-node
                             (next-states-with-cost grid stream-fn)
                             (map (fn [[node cost]]
                                    (when-not (closed-list node)
                                      (let [total-cost (+ total-cost cost)
                                            stored-g (second (open-list node))]
                                        (when (or (nil? stored-g)
                                                  (< total-cost stored-g))
                                          (let [estimate (+ total-cost
                                                            (grid/manhattan-distance (first node)
                                                                                     end))]
                                            [node [estimate total-cost]]))))))
                             (filter some?)
                             (into open-list))]
          (recur open-list closed-list))))))

(defn part-1 [file]
  (solve (read-input file) (fn [xs]
                             (take 3 xs))))

(defn part-2 [file]
  (solve (read-input file) (fn [xs]
                             (->> xs
                                  (drop 3)
                                  (take 7)))))

(comment
  (part-1 "test")
  (time (part-1 "input"))
  (part-2 "test")
  (time (part-2 "input")))