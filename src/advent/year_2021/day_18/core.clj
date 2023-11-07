(ns advent.year-2021.day-18.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-snail-number [line]
  (->> (str/replace line "," "")
       (mapv (fn [ch]
               (case ch
                 \[ :l
                 \] :r
                 (tools/read-int ch))))))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv read-snail-number)))

(defn replace-range [number from count replacement]
  (let [[before starting] (split-at from number)
        [_ after] (split-at count starting)]
    (vec (concat before replacement after))))

(defn maybe-explode [number]
  (when-let [explode-index (loop [depth 0
                                  i 0]
                             (when (< i (count number))
                               (let [depth (case (get number i)
                                             :l (inc depth)
                                             :r (dec depth)
                                             depth)]
                                 (if (= depth 5)
                                   i
                                   (recur depth (inc i))))))]
    (let [pair-left (get number (inc explode-index))
          pair-right (get number (+ explode-index 2))
          find-indexed-number (fn [index]
                                (let [num (get number index)]
                                  (when (number? num)
                                    [index num])))
          [left-index num-left-of] (->> (range explode-index)
                                        reverse
                                        (some find-indexed-number))
          [right-index num-right-of] (->> (range (+ explode-index 4)
                                                 (count number))
                                          (some find-indexed-number))]
      (cond-> number
        right-index
        (replace-range right-index 1 [(+ pair-right num-right-of)])
        true
        (replace-range explode-index 4 [0])
        left-index
        (replace-range left-index 1 [(+ pair-left num-left-of)])))))

(defn maybe-split [number]
  (when-let [[index num] (->> number
                              (map-indexed vector)
                              (some (fn [[i num]]
                                      (when (and (number? num)
                                                 (> num 9))
                                        [i num]))))]
    (let [left (int (Math/floor (/ num 2)))
          right (int (Math/ceil (/ num 2)))]
      (replace-range number index 1 [:l left right :r]))))

(defn add-numbers [x y]
  (loop [number (vec (concat [:l] x y [:r]))]
    (if-let [next-number (or (maybe-explode number)
                             (maybe-split number))]
      (recur next-number)
      number)))

(defn magnitude [number]
  (second
   (reduce (fn [[path result] token]
             (let [next-path (case token
                               :l (conj path 0)
                               :r (let [next-path (pop path)]
                                    (when-let [last-elt (peek next-path)]
                                      (conj (pop next-path)
                                            (inc last-elt))))
                               (conj (pop path)
                                     (inc (peek path))))
                   next-result (if (number? token)
                                 (+ result
                                    (->> path
                                         (map (fn [pos]
                                                (case pos
                                                  0 3
                                                  1 2)))
                                         (apply * token)))
                                 result)]
               [next-path next-result]))
           [[] 0]
           number)))

(defn part-1 [file]
  (->> file
       read-input
       (reduce add-numbers)
       magnitude))

(defn part-2 [file]
  (let [numbers (->> file
                     read-input)]
    (->> (for [x numbers
               y numbers]
           (add-numbers x y))
         (map magnitude)
         (apply max))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))