(ns advent.year-2021.day-16.core
  (:require [clojure.string :as str]
            [advent.tools :as tools]))

(def hex-table
  {\0  "0000"
   \1  "0001"
   \2  "0010"
   \3  "0011"
   \4  "0100"
   \5  "0101"
   \6  "0110"
   \7  "0111"
   \8  "1000"
   \9  "1001"
   \A  "1010"
   \B  "1011"
   \C  "1100"
   \D  "1101"
   \E  "1110"
   \F  "1111"})

(defn hex->bits [s]
  (->> s
       (map hex-table)
       (str/join)))

(defn bits->int [s]
  (Long/parseLong s 2))

(defn read-packet [s]
  (let [bits (hex->bits s)
        read-pos (atom 0)
        read! (fn [n]
                (let [pos @read-pos
                      end-pos (+ pos n)
                      res (subs bits pos end-pos)]
                  (assert (<= end-pos (count bits)))
                  (reset! read-pos end-pos)
                  res))
        read-int! (fn [n]
                    (bits->int (read! n)))
        rec-read (fn rec-read []
                   (let [version (read-int! 3)
                         type-id (read-int! 3)
                         literal? (= type-id 4)
                         data (if literal?
                                (loop [num-bits ""]
                                  (let [repeat? (= "1" (read! 1))
                                        num-bits (str num-bits (read! 4))]
                                    (if repeat?
                                      (recur num-bits)
                                      {:literal (bits->int num-bits)})))
                                (let [abort-fn
                                      (case (read! 1)
                                        "0" (let [total-bits (read-int! 15)
                                                  abort-pos (+ @read-pos total-bits)]
                                              (fn [_]
                                                (= @read-pos abort-pos)))
                                        "1" (let [sub-packets (read-int! 11)]
                                              #{sub-packets}))]
                                  (loop [sub-packets []]
                                    (if (abort-fn (count sub-packets))
                                      {:sub-packets sub-packets}
                                      (recur (conj sub-packets (rec-read)))))))]
                     (merge
                      {:version version
                       :type-id type-id}
                      data)))]
    (rec-read)))

(defn version-sum [{:keys [version sub-packets]}]
  (apply + version (map version-sum sub-packets)))

(defn hex-version-sum [s]
  (version-sum (read-packet s)))

(defn part-1 []
  (hex-version-sum (first (tools/read-lines "input"))))

(defn bin-pred [op]
  (fn [x y]
    (if (op x y)
      1
      0)))

(def operator-map
  {0 +
   1 *
   2 min
   3 max
   5 (bin-pred >)
   6 (bin-pred <)
   7 (bin-pred =)})

(defn evaluate [{:keys [literal sub-packets type-id]}]
  (or literal
      (->> sub-packets
           (map evaluate)
           (apply (operator-map type-id)))))

(defn evaluate-hex [s]
  (evaluate (read-packet s)))

(defn part-2 []
  (evaluate-hex (first (tools/read-lines "input"))))

(comment
  (hex-version-sum "8A004A801A8002F478")
  (part-1)
  (evaluate-hex "C200B40A82")
  (part-2))