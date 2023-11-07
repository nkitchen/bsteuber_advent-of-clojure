(ns advent.year-2021.day-19.core
  (:require [advent.tools :as tools]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-blocks
       (mapv (fn [lines]
               (->> lines
                    next
                    (mapv (fn [line]
                            (->> (str/split line #",")
                                 (mapv tools/read-int)))))))))

(defn all-orientations [[x y z]]
  (->> [[x y z]
        [(- x) (- y) (- z)]]
       (mapcat (fn [[x y z]]
                 [[x y z]
                  [y z x]
                  [z x y]]))
       (mapcat (fn [[x y z]]
                 [[x y z]
                  [x (- z) y]
                  [x (- y) (- z)]
                  [x z (- y)]]))))



(pprint/pprint (all-orientations [1 2 3]))