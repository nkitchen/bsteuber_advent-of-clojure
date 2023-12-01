(ns advent.year-2019.day-22.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (let [tokens (str/split line #" ")]
                 (cond
                   (= tokens ["deal" "into" "new" "stack"])
                   [:deal-into-new-stack]

                   (= (first tokens) "cut")
                   [:cut (tools/read-int (second tokens))]

                   (= (butlast tokens) ["deal" "with" "increment"])
                   [:deal-with-increment (tools/read-int (last tokens))]))))))

(defmulti shuffle-command (fn [_ cmd]
                            (first cmd)))

(defmethod shuffle-command :deal-into-new-stack [cards _]
  (vec (reverse cards)))

(defmethod shuffle-command :cut [cards [_ cut-at]]
  (let [cut-at (if (neg? cut-at)
                 (+ (count cards) cut-at)
                 cut-at)
        [before after] (split-at cut-at cards)]
    (vec (concat after before))))

(defmethod shuffle-command :deal-with-increment [cards [_ increment]]
  (let [num-cards (count cards)]
    (->> cards
         (map-indexed (fn [start-index card]
                        (let [end-index (mod (* start-index increment)
                                             num-cards)]
                          [end-index card])))
         (reduce (fn [cards [end-index card]]
                   (assoc cards end-index card))
                 cards))))

(defn run [file card-count]
  (let [commands (read-input file)
        init-deck (vec (range card-count))]
    (reduce shuffle-command init-deck commands)))

(defn part-1 []
  (let [res (run "input" 10007)]
    (->> res
         (map-indexed vector)
         (some (fn [[index card]]
                 (when (= card 2019)
                   index))))))

(defmulti backwards-position (fn [_ _ cmd]
                               (first cmd)))

(defmethod backwards-position :deal-into-new-stack [pos num-cards _]
  (- num-cards pos 1))

(defmethod backwards-position :cut [pos num-cards [_ cut-at]]
  (let [cut-at (if (neg? cut-at)
                 (+ num-cards cut-at)
                 cut-at)]
    (mod (+ pos cut-at) num-cards)))

(defn extended-euclidean [a b]
  (if (= b 0)
    [a 1 0]
    (let [[g y x] (extended-euclidean b (mod a b))]
      [g x (- y (* x (quot a b)))])))

(defn multiplicative-inverse [a b]
  (let [[g x _] (extended-euclidean a b)]
    (if (= g 1)
      (mod x b)
      (throw (Exception. "Inverse does not exist")))))

(defmethod backwards-position :deal-with-increment [pos num-cards [_ increment]]
  (let [inverse (multiplicative-inverse increment num-cards)]
    (mod (* (bigint pos) (bigint inverse))
         num-cards)))

(def num-cards-part-2 119315717514047)

(defn combined-backwards-position [num-cards file]
  (fn [pos]
    (reduce (fn [pos command]
              (backwards-position pos num-cards command))
            pos
            (reverse (read-input file)))))

(defn part-2 []
  (let [backwards (combined-backwards-position num-cards-part-2 "input")]
    (->> (iterate backwards 2020)
         (take 1000000)
         (map-indexed vector)
         next
         (some (fn [[index card]]
                 (when (= card 2020)
                   index))))))

(comment
  (->> (range 10)
       (mapv (combined-backwards-position 10 "test4")))
  (run "test" 10)
  (run "test2" 10)
  (run "test3" 10)
  (run "test4" 10)
  (part-1)
  (part-2))