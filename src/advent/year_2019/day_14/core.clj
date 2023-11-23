(ns advent.year-2019.day-14.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-amount [s]
  (let [[number chemical] (str/split s #" ")]
    [(tools/read-long number)
     chemical]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (map (fn [line]
              (let [[lhs rhs] (str/split line #" => ")
                    [number chemical] (read-amount rhs)]
                [chemical
                 [number
                  (->> (str/split lhs #", ")
                       (mapv read-amount))]])))
       (into {})))

(defn calc-ore [recipes fuel]
  (loop [required {"FUEL" fuel}]
    (let [really-required (->> required
                               (filter (comp pos? val))
                               (remove (comp #{"ORE"} key)))]
      (if (empty? really-required)
        (required "ORE")
        (let [[chemical number] (first really-required)
              [out-number ingredients] (recipes chemical)
              recipe-times (long (Math/ceil (/ number out-number)))
              required (->> ingredients
                            (cons [(- out-number) chemical])
                            (reduce (fn [required [number chemical]]
                                      (update required chemical (fnil + 0) (* number recipe-times)))
                                    required))]
          (recur required))))))

(defn part-1 [file]
  (calc-ore (read-input file)
            1))

(defn part-2 [file]
  (let [recipes (read-input file)
        upper (->> (iterate #(* 10 %) 1)
                   (drop-while (fn [fuel]
                                 (<= (calc-ore recipes fuel)
                                     1000000000000)))
                   first)
        lower (/ upper 10)]
    (loop [lower lower
           upper upper]
      (let [mid (quot (+ lower upper) 2)]
        (if (= mid lower)
          mid
          (if (<= (calc-ore recipes mid)
                  1000000000000)
            (recur mid upper)
            (recur lower mid)))))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))