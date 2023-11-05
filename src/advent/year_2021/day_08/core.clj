(ns advent.year-2021.day-08.core
  (:require [advent.tools :as tools]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn words [s]
  (str/split s #" "))

(defn read-wiring [line]
  (let [[input output] (str/split line #" \| ")]
    {:input (words input)
     :output (words output)}))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv read-wiring)))

(def digits ["abcefg"
             "cf"
             "acdeg"
             "acdfg"
             "bcdf"
             "abdfg"
             "abdefg"
             "acf"
             "abcdefg"
             "abcdfg"])

(def unique-digit-count?
  (->> digits
       (map count)
       frequencies
       (filter (fn [[_ freq]]
                 (= freq 1)))
       (map key)
       (into #{})))

(defn part-1 [file]
  (->> file
       read-input
       (mapcat :output)
       (map count)
       (filter unique-digit-count?)
       count))

(defn match-digit [possible-mappings digit]
  (prn "match-digit" digit possible-mappings)
  (let [matching (->> digits
                      (filter (fn [actual-digit]
                                (and (= (count digit)
                                        (count actual-digit))
                                     (every? (fn [ch]
                                               (some (possible-mappings ch)
                                                     actual-digit))
                                             digit)
                                     (every? (fn [actual-ch]
                                               (some (fn [ch]
                                                       ((possible-mappings ch)
                                                        actual-ch))
                                                     digit))
                                             actual-digit))))
                      (apply concat)
                      (into #{}))]
    (reduce (fn [possible-mappings ch]
              (update possible-mappings ch set/intersection matching))
            possible-mappings
            digit)))

(defn match-digits [possible-mappings given-digits]
  (reduce match-digit possible-mappings given-digits))

(def used-chars (into #{} "abcdefg"))

(defn rec-solve [possible-mappings first-digit remaining-digits]
  (prn "rec-solve" first-digit remaining-digits possible-mappings)
  (let [possible-mappings (match-digits possible-mappings remaining-digits)]
    (prn "after-match" possible-mappings)
    (cond
      (empty? remaining-digits)
      possible-mappings

      (some empty? (vals possible-mappings))
      (do (prn "fail")
          nil)

      :else
      (let [[first-letter & remaining-letters] (first remaining-digits)]
        (some (fn [target-letter]
                (println first-letter "->" target-letter)
                (let [possible-mappings (-> possible-mappings
                                            (update-vals #(disj % target-letter))
                                            (assoc first-letter #{target-letter}))
                      remaining-digits (if (empty? remaining-letters)
                                         (next remaining-digits)
                                         (cons remaining-letters (next remaining-digits)))]
                  (prn "map aft" possible-mappings)
                  (rec-solve possible-mappings remaining-digits)))
              (possible-mappings first-letter))))))

(defn solve-wiring [{:keys [input output]}]
  (let [given-digits (->> (concat input output)
                          (sort-by count))
        possible-mappings (->> used-chars
                               (map (fn [ch]
                                      [ch used-chars]))
                               (into {}))]
    (rec-solve possible-mappings given-digits)))

(defn part-2 [file]
  (->> file
       read-input
       (take 1)
       (mapv solve-wiring)))

(def sample "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(solve-wiring (read-wiring sample))

(comment
  (part-1 "test")
  (part-1 "input"))