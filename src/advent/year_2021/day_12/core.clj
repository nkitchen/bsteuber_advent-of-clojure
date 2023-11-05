(ns advent.year-2021.day-12.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (let [connections (->> file
                         tools/read-lines
                         (map (fn [line]
                                (str/split line #"-"))))
        small-caves (->> connections
                         (apply concat)
                         (filter (fn [cave]
                                   (= cave (str/lower-case cave))))
                         (into #{}))
        links (->> file
                   tools/read-lines
                   (map (fn [line]
                          (str/split line #"-")))
                   (reduce (fn [links [cave-1 cave-2]]
                             (-> links
                                 (update cave-1 (fnil conj #{}) cave-2)
                                 (update cave-2 (fnil conj #{}) cave-1)))
                           {}))]
    {:small-caves small-caves
     :links links}))

(defn count-paths [{:keys [small-caves links]} part-2?]
  (let [cache (atom {})
        rec-count (fn rec-count [current remaining-small]
                    (let [cache-key [current remaining-small]]
                      (if-let [result (get @cache cache-key)]
                        result
                        (let [result (->> (get links current)
                                          (remove #{"start"})
                                          (map (fn [cave]
                                                 (if (= cave "end")
                                                   1
                                                   (let [small? (small-caves cave)
                                                         remaining? (when small?
                                                                      (remaining-small cave))]
                                                     (if (or (not small?)
                                                             remaining?
                                                             (:any remaining-small))
                                                       (let [mark-visited (when small?
                                                                            (if remaining?
                                                                              cave
                                                                              :any))]
                                                         (rec-count cave (disj remaining-small mark-visited)))
                                                       0)))))
                                          (reduce +))]
                          (swap! cache assoc cache-key result)
                          result))))
        remaining-small (disj small-caves "start")
        remaining-small (if part-2?
                          (conj remaining-small :any)
                          remaining-small)]
    (rec-count "start" remaining-small)))

(defn part-1 [file]
  (count-paths (read-input file) 1))

(defn part-2 [file]
  (count-paths (read-input file) 2))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))