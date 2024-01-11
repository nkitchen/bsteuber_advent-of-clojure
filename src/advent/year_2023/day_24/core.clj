(ns advent.year-2023.day-24
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (->> (str/split line #" @ ")
                    (mapv (fn [nums]
                            (->> (str/split nums #", ")
                                 (mapv tools/read-int)))))))))

;; x1 + t*vx1 = x2 + t*vx2

;; t*(vx1-vx2)=x2-x1
;; t = (x2-x1)/(vx1-vx2)

;; y1 + t*vy1 = y2 + t*vy2