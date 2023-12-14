(ns advent.year-2023.day-06.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (->> (str/split line #" ")
                    next
                    (remove empty?)
                    (mapv tools/read-int))))))

(defn win?-fn [total-time win-distance]
  (fn [wait-time]
    (>
     (* wait-time (- total-time wait-time))
     win-distance)))

(defn part-1 [file]
  (->> file
       read-input
       (apply map (fn [total-time win-distance]
                    (->> (range total-time)
                         (filter (win?-fn total-time win-distance))
                         count)))
       (apply *)))

(defn part-2 [file]
  (let [[total-time win-distance] (->> file
                                       read-input
                                       (map #(apply str %))
                                       (map tools/read-long))
        best-wait-time (quot total-time 2)
        wins? (win?-fn total-time win-distance)]
    (if-not (wins? best-wait-time)
      0
      (loop [low 0
             high best-wait-time]
        (let [mid (quot (+ low high) 2)]
          (if (= mid low)
            (do (prn "RES" high)
                (+ (* 2 (- best-wait-time high))
                   (if (even? total-time)
                     1
                     0)))
            (if (wins? mid)
              (recur low mid)
              (recur mid high))))))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))

;; n * (t-n) = b
;; -n^2 + tn - b = 0
;;  n^2 - tn + b = 0
;; n= t/2 +/- sqrt(t^2/4 - b)