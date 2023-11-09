(ns advent.year-2021.day-25.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  (let [lines (tools/read-lines file)
        cocumbers (->> lines
                       (map-indexed
                        (fn [y line]
                          (->> line
                               (map-indexed
                                (fn [x ch]
                                  (case ch
                                    \v [[x y] :down]
                                    \> [[x y] :right]
                                    \. nil))))))
                       (apply concat)
                       (filter some?)
                       (into {}))]
    {:cocumbers cocumbers
     :width (count (first lines))
     :height (count lines)}))

(defn move-all-right [{:keys [cocumbers width]
                       :as state}]
  (let [res
        (->> cocumbers
             (map (fn [[pos dir]]
                    (let [[x y] pos
                          next-pos [(mod (inc x) width) y]]
                      (if (or (cocumbers next-pos)
                              (= dir :down))
                        [pos dir]
                        [next-pos dir]))))
             (into {}))]
    (assoc state :cocumbers res)))

(defn move-all-down [{:keys [cocumbers height]
                      :as state}]
  (let [res
        (->> cocumbers
             (map (fn [[pos dir]]
                    (let [[x y] pos
                          next-pos [x (mod (inc y) height)]]
                      (if (or (cocumbers next-pos)
                              (= dir :right))
                        [pos dir]
                        [next-pos dir]))))
             (into {}))]
    (assoc state :cocumbers res)))

(defn next-state [state]
  (let [next (-> state
                 move-all-right
                 move-all-down)]
    (when-not (= next state)
      next)))

(defn part-1 [file]
  (->> file
       read-input
       (iterate next-state)
       (take-while some?)
       count))

(comment
  (part-1 "test")
  (part-1 "input"))