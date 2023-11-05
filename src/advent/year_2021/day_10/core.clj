(ns advent.year-2021.day-10.core
  (:require [advent.tools :as tools]))

(defn read-input [file]
  (tools/read-lines file))

(def paren-mapping
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(def illegal-paren-score
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def incomplete-paren-score
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn process-line [line]
  (loop [line line
         stack []]
    (if-let [first-char (first line)]
      (if-let [close-paren (paren-mapping first-char)]
        (recur (next line)
               (conj stack close-paren))
        (if (= (peek stack) first-char)
          (recur (next line)
                 (pop stack))
          {:illegal-paren first-char}))
      (if (empty? stack)
        {:ok true}
        {:incomplete (reverse stack)}))))

(defn part-1 [file]
  (->> file
       read-input
       (map (comp :illegal-paren process-line))
       (filter some?)
       (map illegal-paren-score)
       (apply +)))

(defn score-completion [completion]
  (reduce (fn [score paren]
            (+ (* 5 score)
               (incomplete-paren-score paren)))
          0
          completion))

(defn middle-element [coll]
  (nth (sort coll) (/ (dec (count coll))
                      2)))

(defn part-2 [file]
  (->> file
       read-input
       (map (comp :incomplete process-line))
       (filter some?)
       (map score-completion)
       middle-element))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))