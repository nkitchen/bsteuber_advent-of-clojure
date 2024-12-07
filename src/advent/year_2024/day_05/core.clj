(ns advent.year-2024.day-05.core
  (:require
   [advent.tools :as tools]
   [clojure.string :as str]))

(defn read-input [file]
  (let [[rules updates] (tools/read-blocks file)]
    {:rules (->> rules
                 (map (fn [line]
                        (->> (str/split line #"\|")
                             (mapv tools/read-int))))
                 (into #{}))
     :updates (->> updates
                   (mapv (fn [line]
                           (->> (str/split line #",")
                                (mapv tools/read-int)))))}))

(defn wrong-order? [rules pages]
  (some (fn [[page-1 page-2 page-index-1 page-index-2]]
          (when (rules [page-2 page-1])
            [page-index-1 page-index-2]))
        (for [page-index-1 (range (count pages))
              page-index-2 (range (inc page-index-1) (count pages))]
          (let [page-1 (pages page-index-1)
                page-2 (pages page-index-2)]
            [page-1 page-2 page-index-1 page-index-2]))))

(defn sum-middle-pages [updates]
  (->> updates
       (map (fn [pages]
              (pages (/ (dec (count pages))
                        2))))
       (apply +)))

(defn part-1 [file]
  (let [{:keys [rules updates]} (read-input file)]
    (->> updates
         (remove (partial wrong-order? rules))
         sum-middle-pages)))

(defn fix-order [rules pages]
  (if-let [[page-index-1 page-index-2] (wrong-order? rules pages)]
    (let [[before-1 including-1] (split-at page-index-1 pages)
          [before-2 including-2] (split-at (- page-index-2 page-index-1) including-1)
          next-pages (vec (concat before-1
                                  [(pages page-index-2)]
                                  before-2
                                  (rest including-2)))]
      (recur rules next-pages))
    pages))

(defn part-2 [file]
  (let [{:keys [rules updates]} (read-input file)]
    (->> updates
         (filter (partial wrong-order? rules))
         (map (partial fix-order rules))
         sum-middle-pages)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
