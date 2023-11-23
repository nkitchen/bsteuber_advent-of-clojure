(ns advent.year-2019.day-04.core
  (:require [advent.tools :as tools]))

(defn check-password [part-2? number]
  (let [numbers (map tools/read-int (str number))]
    (loop [prev nil
           current (first numbers)
           remaining (next numbers)
           had-same? false]
      (if-let [next-number (first remaining)]
        (when (>= next-number current)
          (let [had-same? (or had-same?
                              (and
                               (= next-number current)
                               (or (not part-2?)
                                   (and (not= prev current)
                                        (not= (second remaining) current)))))]
            (recur current next-number (next remaining) had-same?)))
        had-same?))))

(defn solve [part-2?]
  (->> (range 271973 785962)
       (filter (partial check-password part-2?))
       count))

(def part-1 (partial solve false))
(def part-2 (partial solve true))

(comment
  (check-password true 123455)
  (part-1)
  (part-2))