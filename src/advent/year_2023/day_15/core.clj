(ns advent.year-2023.day-15.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapcat (fn [line]
                 (str/split line #",")))
       vec))

(defn calc-hash [s]
  (reduce (fn [prev-hash ch]
            (-> prev-hash
                (+ (int ch))
                (* 17)
                (mod 256)))
          0
          s))

(defn part-1 [file]
  (->> file
       read-input
       (map calc-hash)
       (apply +)))

(defn set-label [labels label value]
  (let [labels (or labels [])
        num-labels (count labels)
        index (or (->> (range num-labels)
                       (some (fn [index]
                               (when (= (first (labels index))
                                        label)
                                 index))))
                  num-labels)]
    (assoc labels index [label value])))

(defn remove-label [labels label]
  (->> labels
       (remove #(= (first %) label))
       vec))

(defn apply-command [boxes s]
  (let [[label arg] (str/split s #"[-=]")
        hash (calc-hash label)]
    (if arg
      (update boxes hash set-label label (tools/read-int arg))
      (update boxes hash remove-label label))))

(defn part-2 [file]
  (->> file
       read-input
       (reduce apply-command {})
       (mapcat (fn [[box-num labels]]
                 (->> labels
                      (map-indexed (fn [slot-num [_ value]]
                                     (* (inc box-num) (inc slot-num) value))))))
       (apply +)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))