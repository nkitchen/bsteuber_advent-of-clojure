(ns advent.year-2021.day-17.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (let [line (first (tools/read-lines file))
        assignments (-> line
                        (str/replace "target area: " "")
                        (str/split #", "))]
    (->> assignments
         (map (fn [assignment]
                (let [[var minmax] (str/split assignment #"=")
                      [min max] (->> (str/split minmax #"\.\.")
                                     (map tools/read-int))
                      min-key (keyword (str var "min"))
                      max-key (keyword (str var "max"))]
                  {min-key min
                   max-key max})))
         (apply merge))))

(defn simulate [vx vy {:keys [xmin xmax ymin ymax]}]
  (loop [x 0
         y 0
         vx vx
         vy vy
         highest-y 0]
    (cond
      (and (<= xmin x xmax)
           (<= ymin y ymax))
      (max highest-y y)

      (or (> (Math/abs x)
             (max
              (Math/abs xmax)
              (Math/abs xmin)))
          (and (< y ymin)
               (neg? vy)))
      nil

      :else (recur  (+ x vx)
                    (+ y vy)
                    (cond
                      (pos? vx) (dec vx)
                      (neg? vx) (inc vx)
                      :else     vx)
                    (dec vy)
                    (max highest-y y)))))



(defn part-1 [file]
  (let [target-region (read-input file)]
    (->> (for [x (range 0 (inc (:xmax target-region)))
               y (range (:ymin target-region) 117)]
           (simulate x y target-region))
         (filter some?)
         (apply max))))

(defn part-2 [file]
  (let [target-region (read-input file)]
    (->> (for [x (range 0 (inc (:xmax target-region)))
               y (range (:ymin target-region) 117)]
           (simulate x y target-region))
         (filter some?)
         count)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))