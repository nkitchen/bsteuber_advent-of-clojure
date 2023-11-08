(ns advent.year-2021.day-20.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (let [[enhancement image] (tools/read-blocks file)
        enhancement (mapv (fn [ch]
                            (case ch
                              \. false
                              \# true))
                          (first enhancement))
        pixels (->> image
                    (map-indexed (fn [y line]
                                   (map-indexed (fn [x ch]
                                                  (when (= ch \#)
                                                    [x y]))
                                                line)))
                    (apply concat)
                    (filter some?)
                    (into #{}))]
    {:enhancement enhancement
     :image {:pixels pixels
             :outside-dark? false
             :start 0
             :end (count image)}}))

(defn get-pixel [{:keys [pixels outside-dark? start end]} [x y :as point]]
  (if (and (<= start x end)
           (<= start y end))
    (get pixels point)
    outside-dark?))

(defn surrounding [[px py]]
  (for [y [(dec py) py (inc py)]
        x [(dec px) px (inc px)]]
    [x y]))

(defn as-binary-number [bits]
  (->> bits
       (map (fn [bit]
              (if bit "1" "0")))
       str/join
       tools/read-binary))

(defn calc-next-pixel [enhancement image point]
  (->> point
       surrounding
       (map #(get-pixel image %))
       as-binary-number
       enhancement))

(defn next-image-points [{:keys [start end]}]
  (for [x (range (dec start) (+ end 2))
        y (range (dec start) (+ end 2))]
    [x y]))

(defn enhance-image [enhancement {:keys [start end outside-dark?]
                                  :as image}]
  (let [pixels (->> (next-image-points image)
                    (map (fn [point]
                           (when (calc-next-pixel enhancement image point)
                             point)))
                    (filter some?)
                    (into #{}))]
    {:pixels pixels
     :start (dec start)
     :end (inc end)
     :outside-dark? (if outside-dark?
                      (enhancement 511)
                      (enhancement 0))}))

(defn solver [enhance-steps]
  (fn [file]
    (let [{:keys [enhancement image]} (read-input file)]
      (->>  image
            (iterate (partial enhance-image enhancement))
            (drop enhance-steps)
            first
            :pixels
            count))))

(def part-1 (solver 2))
(def part-2 (solver 50))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))