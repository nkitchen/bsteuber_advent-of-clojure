(ns advent.year-2024.day-13.core
  (:require
   [advent.tools :as tools]))

(defn read-input [file]
  (->> file
       tools/read-blocks
       (mapv (fn [lines]
               (->> lines
                    (mapcat #(re-seq #"\d+" %))
                    (mapv tools/read-long))))))

;; 1) xa * a + xb * b = x
;; 2) ya * a + yb * b = y
;;
;; 1) * ya - 2) * xa
;;
;; (xb * ya - yb * xa) * b = x * ya - y * xa
;; b = ( x * ya - y * xa ) / (xb * ya - yb * xa)
;;
;; if xb * ya - yb * xa is zero, the equations are linear dependent.
;; If x * ya - y * xa is also zero, there are infinitely many solutions,
;; which fortunately doesn't happen.
;; Otherwise, there are no solutions.

(defn natural-number? [n]
  (and (integer? n)
       (>= n 0)))

(defn solve [part-2? [xa ya xb yb x y]]
  (let [x (if part-2?
            (+ x 10000000000000)
            x)
        y (if part-2?
            (+ y 10000000000000)
            y)
        b-factor (- (* xb ya) (* yb xa))
        rhs (- (* x ya) (* y xa))]
    (if (zero? b-factor)
      (when (zero? rhs)
        (throw (ex-info "Multiple Solutions!" {})))
      (let [b (/ rhs b-factor)
            a (/ (- x (* xb b))
                 xa)]
        (when (and (natural-number? a)
                   (natural-number? b))
          [a b])))))

(defn cost [[a b]]
  (+ (* 3 a)
     b))

(defn run [part-2? file]
  (->> (read-input file)
       (map (partial solve part-2?))
       (filter some?)
       (map cost)
       (apply +)))

(def part-1 (partial run false))
(def part-2 (partial run true))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
