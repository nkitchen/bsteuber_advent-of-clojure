(ns advent.year-2024.day-22.core
  (:require
   [advent.tools :as tools]
   [clojure.test :refer [deftest is]]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (map tools/read-long)))

(def mix bit-xor)

(defn prune [x]
  (mod x 16777216))

(defn calc-step [secret left-shift]
  (let [shifted (if (pos? left-shift)
                  (bit-shift-left secret left-shift)
                  (bit-shift-right secret (- left-shift)))]
    (prune (mix secret shifted))))

(defn next-secret [secret]
  (-> secret
      (calc-step 6)
      (calc-step -5)
      (calc-step 11)))

(deftest test-next-secret
  (is (= [15887950
          16495136
          527345
          704524
          1553684
          12683156
          11100544
          12249484
          7753432
          5908254]
         (->> (iterate next-secret 123)
              (rest)
              (take 10)))))

(defn part-1 [file]
  (->> file
       read-input
       (map (fn [secret]
              (nth (iterate next-secret secret)
                   2000)))
       (apply +)))

(defn analyze-change-sequences [secret]
  (->> (iterate next-secret secret)
       (take 2001)
       (map (fn [secret]
              (mod secret 10)))
       (partition 5 1)
       (map (fn [prices]
              (let [changes (->> prices
                                 (partition 2 1)
                                 (mapv (fn [[x y]]
                                         (- y x))))
                    price (last prices)]
                [changes price])))
       (reduce (fn [m [changes price]]
                 (if (m changes)
                   m
                   (assoc m changes price)))
               {})))

(defn part-2 [file]
  (->> file
       read-input
       (map analyze-change-sequences)
       (apply merge-with +)
       (map val)
       (apply max)))

(comment
  (part-1 "test1")
  (part-1 "input")
  (part-2 "test2")
  (time (part-2 "input")))
