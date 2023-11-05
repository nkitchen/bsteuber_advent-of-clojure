(ns advent.year-2021.day-13.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (let [[dots folds] (tools/read-blocks file)]
    {:dots (->> dots
                (map (fn [line]
                       (->> (str/split line #",")
                            (mapv tools/read-int))))
                (into #{}))
     :folds (->> folds
                 (map (fn [line]
                        (let [assignment (last (str/split line #" "))
                              [axis value] (str/split assignment #"=")]
                          {:axis axis
                           :value (tools/read-int value)}))))}))

(defn fold-dot [{:keys [axis value]} [x y]]
  (case axis
    "x" (if (> x value)
          [(+ value value (- x)) y]
          [x y])
    "y" (if (> y value)
          [x (+ value value (- y))]
          [x y])))

(defn apply-fold [dots fold]
  (->> dots
       (map (partial fold-dot fold))
       (into #{})))

(defn print-dots [dots]
  (let [max-x (->> dots
                   (map first)
                   (apply max))
        max-y (->> dots
                   (map second)
                   (apply max))]
    (doseq [y (range (inc max-y))]
      (doseq [x (range (inc max-x))]
        (print (if (dots [x y])
                 "#"
                 ".")))
      (println))))

(defn part-1 [file]
  (let [{:keys [dots folds]} (read-input file)]
    (->> folds
         first
         (apply-fold dots)
         count)))

(defn part-2 [file]
  (let [{:keys [dots folds]} (read-input file)
        dots (reduce apply-fold dots folds)]
    (print-dots dots)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))