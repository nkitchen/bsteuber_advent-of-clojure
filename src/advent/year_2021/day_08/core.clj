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

(defn lookup-digit [s]
  (let [s (apply str (sort s))]
    (->> digits
         (map-indexed vector)
         (some (fn [[i letters]]
                 (when (= s letters)
                   i))))))

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
  (let [possible-mappings (match-digits possible-mappings remaining-digits)]
    (cond
      (nil? first-digit)
      possible-mappings

      (some empty? (vals possible-mappings))
      nil

      :else
      (let [[first-letter & remaining-letters] first-digit]
        (some (fn [target-letter]
                (let [possible-mappings (-> possible-mappings
                                            (update-vals #(disj % target-letter))
                                            (assoc first-letter #{target-letter}))
                      remaining-digits (if (empty? remaining-letters)
                                         (next remaining-digits)
                                         remaining-digits)
                      continue-first? (seq remaining-letters)
                      next-first (if continue-first?
                                   remaining-letters
                                   (first remaining-digits))
                      next-remaining (if continue-first?
                                       remaining-digits
                                       (next remaining-digits))]
                  (rec-solve possible-mappings next-first next-remaining)))
              (possible-mappings first-letter))))))

(defn solve-wiring [{:keys [input output]}]
  (let [given-digits (->> (concat input output)
                          (sort-by count))
        possible-mappings (->> used-chars
                               (map (fn [ch]
                                      [ch used-chars]))
                               (into {}))
        solution (rec-solve (match-digits possible-mappings given-digits)
                            (first given-digits)
                            (next given-digits))]
    (update-vals solution first)))

(defn part-2 [file]
  (->> file
       read-input
       (map (fn [data]
              (let [wiring (solve-wiring data)
                    out-digits (->> data
                                    :output
                                    (mapv (fn [out-letters]
                                            (->> out-letters
                                                 (map wiring)
                                                 (apply str)
                                                 lookup-digit))))]
                (->> out-digits
                     (apply str)
                     tools/read-int))))
       (apply +)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test2")
  (part-2 "test")
  (part-2 "input"))