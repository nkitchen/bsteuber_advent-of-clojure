(ns advent.year-2024.day-07.core
  (:require
   [advent.tools :as tools]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (let [[result & inputs] (->> (re-seq #"\d+" line)
                                            (map tools/read-long))]
                 [result (vec inputs)])))))

(defn rec-solve [ops result current [fst & more]]
  (cond
    (> current result) false
    (not fst)          (= current result)
    :else              (some (fn [op]
                               (rec-solve ops result (op current fst) more))
                             ops)))

(defn solvable? [ops [result [fst & more]]]
  (rec-solve ops result fst more))

(defn run [ops file]
  (->> file
       read-input
       (filter (partial solvable? ops))
       (map first)
       (apply +)))

(def part-1 (partial run [* +]))

(defn || [x y]
  (tools/read-long (str x y)))

(def part-2 (partial run [|| * +]))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
