(ns advent.year-2019.day-08.core
  (:require [advent.tools :as tools]))

(defn count-digits [digit layer]
  (->> layer
       (filter #{digit})
       count))

(defn part-1 [width height data]
  (let [layers (partition (* width height) data)
        fewest-0-layer (->> layers
                            (apply min-key (partial count-digits \0)))]
    (* (count-digits \1 fewest-0-layer)
       (count-digits \2 fewest-0-layer))))

(def input (first (tools/read-lines "input")))

(defn part-2 [width height data]
  (let [layers (partition (* width height) data)
        image (->> layers
                   (apply map (fn [& pixels]
                                (->> pixels
                                     (remove #{\2})
                                     first)))
                   vec)]
    (doseq [row (range height)]
      (doseq [col (range width)]
        (print
         (case (image (+ col (* row width)))
           \0 " "
           \1 "#")))
      (println))))

(comment
  (part-1 25 6 input)
  (part-2 2 2 "0222112222120000")
  (part-2 25 6 input))