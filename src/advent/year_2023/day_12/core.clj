(ns advent.year-2023.day-12.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (let [[row numbers] (str/split line #" ")]
                 [row (->> (str/split numbers #",")
                           (mapv tools/read-int))])))))

(defn min-row-len [numbers]
  (+ (apply + numbers)
     (count numbers)
     -1))

(defn enumerate-row [[row numbers]]
  (let [cache (atom {})

        rec-enumerate
        (fn rec-enumerate [row numbers]
          (let [row-count (count row)
                cache-key [row-count (count numbers)]]
            (if-let [res (@cache cache-key)]
              res
              (let [res
                    (cond
                      (< row-count (min-row-len numbers))
                      0

                      (empty? row)
                      (if (seq numbers)
                        0
                        1)

                      :else
                      (let [ch (first row)]
                        (+ (if (#{\? \.} ch)
                             (rec-enumerate (next row) numbers)
                             0)
                           (or
                            (when (#{\? \#} ch)
                              (when-let [[next-number & more-numbers] numbers]
                                (let [[block-chs more-chs] (split-at next-number row)
                                      [spacer & more-chs] more-chs]
                                  (when (and (= (count block-chs) next-number)
                                             (not (some #{\.} block-chs))
                                             (not= \# spacer))
                                    (rec-enumerate more-chs more-numbers)))))
                            0))))]
                (swap! cache assoc cache-key res)
                res))))]
    (rec-enumerate row numbers)))

(defn part-1 [file]
  (->> file
       read-input
       (map enumerate-row)
       (apply +)))

(defn unfold-records [[row numbers]]
  [(->> (repeat 5 row)
        (str/join "?"))
   (->> (repeat 5 numbers)
        (apply concat))])

(defn part-2 [file]
  (->> file
       read-input
       (map unfold-records)
       (map enumerate-row)
       (apply +)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))