(ns advent.year-2024.day-11.core
  (:require
   [advent.tools :as tools]))

(def test-data [125 17])
(def input-data [475449 2599064 213 0 2 65 5755 51149])


(defn evolve [num]
  (if (zero? num)
    (list 1)
    (let [num-str (str num)
          digits (count num-str)]
      (if (even? digits)
        (let [half-digits (/ digits 2)]
          [(tools/read-long (subs num-str 0 half-digits))
           (tools/read-long (subs num-str half-digits))])
        (list (* num 2024))))))


(defn solve [blink-times nums]
  (let [cache (atom {})
        rec-solve (fn rec-solve [blink-times num]
                    (or (@cache [blink-times num])
                        (let [res (if (zero? blink-times)
                                    1
                                    (->> (evolve num)
                                         (map #(rec-solve (dec blink-times) %))
                                         (apply +)))]
                          (swap! cache assoc [blink-times num] res)
                          res)))]
    (->> nums
         (map #(rec-solve blink-times %))
         (apply +))))

(def part-1 (partial solve 25))
(def part-2 (partial solve 75))



(comment
  (part-1 test-data)
  (part-1 input-data)
  (part-2 test-data)
  (part-2 input-data))
