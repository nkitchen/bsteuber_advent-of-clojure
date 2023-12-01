(ns advent.year-2023.day-01.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn part-1 [file]
  (->> file
       tools/read-lines
       (map (fn [line]
              (let [numbers (->> line
                                 (filter (fn [ch]
                                           (<= (int \0)
                                               (int ch)
                                               (int \9)))))]
                (tools/read-int (str (first numbers)
                                     (last numbers))))))
       (apply +)))

(def digits
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(defn part-2 [file]
  (->> file
       tools/read-lines
       (map (fn [line]
              (let [numbers (->> (range (count line))
                                 (map (fn [i]
                                        (let [ch (nth line i)
                                              s (subs line i)]
                                          (if (<= (int \0)
                                                  (int ch)
                                                  (int \9))
                                            ch
                                            (some (fn [[digit-str digit]]
                                                    (when (str/starts-with? s digit-str)
                                                      digit))
                                                  digits)))))
                                 (filter some?))]
                (tools/read-int (str (first numbers)
                                     (last numbers))))))
       (apply +)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test2")
  (part-2 "input"))