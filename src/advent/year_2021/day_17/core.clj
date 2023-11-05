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
                      [min max] (str/split minmax #"\.\.")
                      min-key (keyword (str var "min"))
                      max-key (keyword (str var "max"))]
                  {min-key min
                   max-key max})))
         (apply merge))))

(read-input "test")