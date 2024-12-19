(ns advent.year-2024.day-19.core
  (:require
   [advent.tools :as tools]
   [clojure.string :as str]))

(defn read-input [file]
  (let [[[towels] designs] (tools/read-blocks file)]
    [(str/split towels #", ")
     designs]))

(defn count-combinations [towels design]
  (let [cache (atom {"" 1})
        rec-solve (fn rec-solve [s]
                    (if-let [res (@cache s)]
                      res
                      (let [res (->> towels
                                     (map (fn [towel]
                                            (when (str/starts-with? s towel)
                                              (let [next-s (subs s (count towel))]
                                                (rec-solve next-s)))))
                                     (filter some?)
                                     (apply + 0))]
                        (swap! cache assoc s res)
                        res)))]
    (rec-solve design)))

(defn part-1 [file]
  (let [[towels designs] (read-input file)]
    (->> designs
         (map (partial count-combinations towels))
         (filter pos?)
         count)))

(defn part-2 [file]
  (let [[towels designs] (read-input file)]
    (->> designs
         (map (partial count-combinations towels))
         (apply +))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
