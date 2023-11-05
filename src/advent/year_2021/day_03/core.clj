(ns advent.year-2021.day-03.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  (->> (tools/read-lines file)
       vec))

(defn freqs-at [index binaries]
  (->> binaries
       (map #(nth % index))
       frequencies))

(defn find-most-common
  [freqs]
  (->> freqs
       (sort-by second >)
       (ffirst)))

(defn find-least-common
  [freqs]
  (->> freqs
       (sort-by second <)
       (ffirst)))

(defn same-freq? [freqs]
  (and (= 2 (count freqs))
       (->> freqs
            (map second)
            (apply =))))

(defn part-1 [file]
  (let [binaries (read-input file)]
    (->> (range (count (first binaries)))
         (map (fn [index]
                (let [freqs (freqs-at index binaries)]
                  [(find-most-common freqs)
                   (find-least-common freqs)])))
         (apply map str)
         (map tools/read-binary)
         (apply *))))

(defn finder [pick-fn default]
  (fn [binaries]
    (loop [index 0
           binaries binaries]
      (if (= 1 (count binaries))
        (tools/read-binary (first binaries))
        (let [_ (assert (< index (count (first binaries)))
                        (str index " " (vec binaries)))
              freqs  (freqs-at index binaries)
              keep-bit (if (same-freq? freqs)
                         default
                         (pick-fn freqs))
              binaries (->> binaries
                            (filter (fn [bin]
                                      (= (nth bin index) keep-bit))))]
          (recur (inc index) binaries))))))

(def find-oxygen-rating (finder find-most-common \1))
(def find-co2-rating (finder find-least-common \0))

(defn part-2 [file]
  (let [binaries (read-input file)
        oxygen (find-oxygen-rating binaries)
        co2 (find-co2-rating binaries)]
    (* oxygen co2)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))