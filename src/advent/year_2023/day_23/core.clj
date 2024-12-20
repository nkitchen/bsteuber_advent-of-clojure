(ns advent.year-2023.day-23.core
  (:require [advent.tools :as tools]
            [advent.grid :as grid]))

(defn read-input [file]
  (let [{:keys [rows cols]} (tools/read-grid-dimensions file)
        grid (tools/read-grid file
                              (fn [ch]
                                (case ch
                                  \. grid/directions-4
                                  \> [grid/east]
                                  \v [grid/south]
                                  \< [grid/west]
                                  \^ [grid/north]
                                  nil)))
        start [1 0]
        end [(- cols 2) (- rows 1)]]
    {:start start
     :end end
     :grid grid}))

(defn neighbours [grid forbidden point]
  (->> (grid point)
       (map #(grid/go-dir point %))
       (filter grid)
       (remove forbidden)))

(defn reachable-points [grid forbidden point]
  (loop [seen #{}
         explore [point]]
    (if (empty? explore)
      seen
      (recur (into seen explore)
             (->> explore
                  (mapcat #(neighbours
                            grid
                            forbidden
                            %))
                  (remove seen)
                  distinct)))))

(defn solve [{:keys [grid start end]}]
  (let [cache (atom {})
        rec-solve (fn rec-solve [seen current]
                    (if (= current end)
                      0
                      (let [;reachable (reachable-points grid seen current)
                            cache-key [current seen]
                            seen (conj seen current)]
                        (when true #_(reachable end)
                              (or (get @cache cache-key)
                                  (let [res (->> (neighbours grid seen current)
                                                 (map #(rec-solve seen %))
                                                 (filter some?))
                                        res (when (seq res)
                                              (inc (apply max res)))]
                                    (swap! cache assoc cache-key res)
                                    res))))))]
    (rec-solve #{} start)))

(defn part-1 [file]
  (solve (read-input file)))

(comment
  (part-1 "test")
  (part-1 "input"))
