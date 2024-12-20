(ns advent.year-2024.day-20.core
  (:require
   [advent.grid :as grid]))

(defn read-input [file]
  (grid/read-from-file file (fn [ch]
                              (case ch
                                \# :wall
                                \S [:extra :start]
                                \E [:extra :end]
                                nil))))



(defn calc-path-lengths [{:keys [start grid]
                          :as data}]
  (loop [length 0
         explore [start]
         path-lengths {}]
    (if (empty? explore)
      path-lengths
      (let [path-lengths (merge-with min path-lengths (zipmap explore (repeat length)))
            explore (->> explore
                         (mapcat (fn [point]
                                   (->> grid/directions-4
                                        (map (fn [dir]
                                               (let [neighbor (grid/go-dir point dir)]
                                                 (when (and (grid/on-grid? data neighbor)
                                                            (not= :wall (grid neighbor)))
                                                   neighbor))))
                                        (remove path-lengths)
                                        (filter some?))))
                         (distinct))]
        (recur (inc length) explore path-lengths)))))

(defn calc-cheat-paths [from-start from-end best-normal-length max-cheat-length]
  (->> from-start
       (mapcat (fn [[[px py :as cheat-start] steps-from-start]]
                 (for [x (range (- px max-cheat-length) (+ px max-cheat-length 1))
                       y (range (- py max-cheat-length) (+ py max-cheat-length 1))]
                   (let [cheat-end [x y]
                         distance (grid/manhattan-distance cheat-start cheat-end)]
                     (when (<= distance max-cheat-length)
                       (let [steps-from-end (from-end cheat-end)]
                         (when steps-from-end
                           (let [saving (- best-normal-length (+ steps-from-start
                                                                 steps-from-end
                                                                 distance))]
                             (when (pos? saving)
                               saving)))))))))
       (filter some?)
       frequencies
       sort))

(defn calc-file-cheats [max-cheat-length file]
  (let [{:keys [end]
         :as data} (read-input file)
        from-start (calc-path-lengths data)
        from-end (calc-path-lengths (assoc data :start end))
        best-normal-length (from-start end)]
    (calc-cheat-paths from-start from-end best-normal-length max-cheat-length)))

(defn run [max-cheat-length file]
  (->> file
       (calc-file-cheats max-cheat-length)
       (map (fn [[save-amount cheat-count]]
              (if (>= save-amount 100)
                cheat-count
                0)))
       (apply +)))

(def part-1 (partial run 2))
(def part-2 (partial run 20))

(comment
  (calc-file-cheats "test" 2)
  (part-1 "input")
  (->> (calc-file-cheats "test" 20)
       (filter (fn [[save-amount _]]
                 (>= save-amount 50))))
  (time (part-2 "input")) ; takes 70s on my machine
  )
