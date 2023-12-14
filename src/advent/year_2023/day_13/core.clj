(ns advent.year-2023.day-13.core
  (:require [advent.tools :as tools]))

(defn find-symmetry-part-1 [rows]
  (->> (range 1 (count rows))
       (some (fn [i]
               (let [[before after] (split-at i rows)]
                 (when (->> (map =
                                 (reverse before)
                                 after)
                            (every? identity))
                   i))))))

(defn find-symmetry-part-2 [rows]
  (->> (range 1 (count rows))
       (some (fn [i]
               (let [[before after] (split-at i rows)]
                 (loop [differences 0
                        row-pairs (map vector (reverse before)
                                       after)]
                   (cond
                     (> differences 1)
                     nil

                     (empty? row-pairs)
                     (when (= differences 1)
                       i)

                     :else
                     (let [diffs (->> (apply map = (first row-pairs))
                                      (remove identity)
                                      count)]
                       (recur (+ differences diffs)
                              (next row-pairs))))))))))

(defn solve [find-symmetry file]
  (->> file
       tools/read-blocks
       (map (fn [rows]
              (if-let [i (find-symmetry rows)]
                (* i 100)
                (find-symmetry (apply map str rows)))))
       (apply +)))

(def part-1 (partial solve find-symmetry-part-1))

(def part-2 (partial solve find-symmetry-part-2))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))