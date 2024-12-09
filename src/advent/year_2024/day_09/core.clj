(ns advent.year-2024.day-09.core
  (:require
   [advent.tools :as tools]))

(defn read-input [file]
  (->> file
       tools/read-lines
       first
       (mapv tools/read-long)))

(defn expand-input [numbers]
  (->> numbers
       (map-indexed (fn [i num]
                      (repeat num
                              (when (even? i)
                                (/ i 2)))))
       (apply concat)
       vec))

(defn part-1 [file]
  (let [data (expand-input (read-input file))]
    (loop [sum 0
           left-index 0
           right-index (dec (count data))]
      (cond
        (> left-index right-index)
        sum

        (data left-index)
        (recur (+ sum (* left-index (data left-index)))
               (inc left-index)
               right-index)

        (nil? (data right-index))
        (recur sum left-index (dec right-index))

        :else
        (recur (+ sum (* left-index (data right-index)))
               (inc left-index)
               (dec right-index))))))

(defn calc-checksum [data]
  (->> data
       (map-indexed (fn [i num]
                      (when num
                        (* i num))))
       (filter some?)
       (apply +)))

(defn first-different-index-from-right [data idx value]
  (->> (iterate dec idx)
       rest
       (some (fn [idx]
               (when-not (= (data idx) value)
                 idx)))))

(defn find-space [data file-size]
  (loop [remaining file-size
         index 0]
    (cond
      (zero? remaining)
      (dec index)

      (= index (count data))
      nil

      (nil? (data index))
      (recur (dec remaining) (inc index))

      :else
      (recur file-size (inc index)))))

(defn move-blocks [data right-index target-index file-size]
  (if (zero? file-size)
    data
    (recur (-> data
               (assoc target-index (data right-index))
               (assoc right-index nil))
           (dec right-index)
           (dec target-index)
           (dec file-size))))

(defn part-2 [file]
  (let [data (expand-input (read-input file))]
    (loop [data data
           right-index (dec (count data))]
      (let [value (data right-index)]
        (cond
          (nil? value)
          (recur data (first-different-index-from-right data right-index nil))

          (zero? value)
          (calc-checksum data)

          :else
          (let [next-index (first-different-index-from-right data right-index value)
                file-size (- right-index next-index)
                target-index (find-space data file-size)
                data (if (and target-index
                              (< target-index right-index))
                       (move-blocks data right-index target-index file-size)
                       data)]
            (recur data next-index)))))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
