(ns advent.year-2019.day-17.core
  (:require [advent.year-2019.intcode :as intcode]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]))

(defn calc-map-str []
  (-> (intcode/read-input-program)
      (intcode/run-until-halt [])
      :output
      (->>
       (map char)
       (apply str))))

(defn read-map [s]
  (->> s
       str/split-lines
       (map-indexed (fn [y line]
                      (->> line
                           (map-indexed (fn [x ch]
                                          [[x y] ch]))
                           (remove (comp #{\.} second)))))
       (apply concat)
       (reduce (fn [state [point ch]]
                 (cond-> state
                   true
                   (update :scaffolds conj point)

                   (not= ch \#)
                   (merge {:robot-pos point
                           :robot-dir (case ch
                                        \< :left
                                        \> :right
                                        \v :down
                                        \^ :up)})))
               {:scaffolds #{}})))


(def directions
  {:left [-1 0]
   :right [1 0]
   :up [0 -1]
   :down [0 1]})

(defn neighbours [point]
  (->> directions
       vals
       (map #(mapv + point %))))

(def test-map
  (read-map
   "..#..........
..#..........
#######...###
#.#...#...#.#
#############
..#...#...#..
..#####...^.."))

(def test-map-2
  (read-map
   "#######...#####
#.....#...#...#
#.....#...#...#
......#...#...#
......#...###.#
......#.....#.#
^########...#.#
......#.#...#.#
......#########
........#...#..
....#########..
....#...#......
....#...#......
....#...#......
....#####......"))

(def input-map (read-map (calc-map-str)))

(defn part-1 [m]
  (let [{:keys [scaffolds]} m
        intersections (->> scaffolds
                           (filter (fn [point]
                                     (->> point
                                          neighbours
                                          (every? scaffolds)))))]
    (->> intersections
         (map (partial apply *))
         (apply +))))

(defn rotate [dir turn]
  (case turn
    "R" (case dir
          :left :up
          :down :left
          :right :down
          :up :right)
    "L" (case dir
          :up :left
          :left :down
          :down :right
          :right :up)
    dir))

(defn go-step [pos dir]
  (mapv + pos (directions dir)))

(defn calc-path [{:keys [scaffolds
                         robot-pos
                         robot-dir]}]
  (loop [path []
         steps 0
         pos robot-pos
         dir robot-dir]
    (if-let [[turn dir pos] (->> [nil "L" "R"]
                                 (some (fn [turn]
                                         (let [dir (rotate dir turn)
                                               pos (go-step pos dir)]
                                           (when (scaffolds pos)
                                             [turn dir pos])))))]
      (if turn
        (let [path (if (pos? steps)
                     (conj path steps)
                     path)]
          (recur (conj path turn)
                 1
                 pos
                 dir))
        (recur path
               (inc steps)
               pos
               dir))
      (conj path steps))))

(defn path-str [path]
  (->> path
       (str/join ",")))

(def max-chars 20)

(defn find-prefixes [path]
  (->> (range 2 (min (dec (count path))
                     11))
       (map (fn [len]
              (let [[path-1 path-2] (split-at len path)]
                [(vec path-1) (vec path-2)])))
       (filter (fn [[path-1 path-2]]
                 (and (<= (count (path-str path-1)) max-chars)
                      (seq path-2))))))

(defn try-subroutine-combination [subroutines path]
  (let [[sub-1 sub-2 sub-3] subroutines
        labelled-routines [[sub-1 "A"]
                           [sub-2 "B"]
                           [sub-3 "C"]]
        rec-try (fn rec-try [path]
                  (let [path-len (count path)]
                    (if (zero? path-len)
                      []
                      (some (fn [[routine letter]]
                              (let [routine-len (count routine)
                                    last-index (dec routine-len)]
                                (when (and (<= routine-len path-len)
                                           (every? (fn [index]
                                                     (= (path index)
                                                        (routine index)))
                                                   (range last-index)))
                                  (let [path-last (path last-index)
                                        routine-last (routine last-index)
                                        next-path (drop routine-len path)
                                        next-path (cond
                                                    (= path-last routine-last)
                                                    next-path

                                                    (and (number? path-last)
                                                         (number? routine-last)
                                                         (< routine-last path-last))
                                                    (let [num-rest (- path-last routine-last)]
                                                      (cons num-rest next-path)))]
                                    (when next-path
                                      (when-let [result (rec-try (vec next-path))]
                                        (cons letter result)))))))
                            labelled-routines))))]
    (when-let [main-routine (rec-try path)]
      (let [main-str (path-str main-routine)]
        (when (<= (count main-str) max-chars)
          (cons main-str (map path-str subroutines)))))))

(defn find-sub-paths [path]
  (let [rec-sub-paths (fn rec-sub-paths [sub-paths path]
                        (if (= (count sub-paths) 3)
                          [sub-paths]
                          (->> (find-prefixes path)
                               doall
                               (mapcat (fn [[prefix path]]
                                         (rec-sub-paths (conj sub-paths prefix)
                                                        path))))))]
    (map vec (rec-sub-paths #{} path))))

(defn solve [path]
  (->> (find-sub-paths path)
       (some #(try-subroutine-combination % path))))

(defn part-2 [m]
  (let [program (solve (calc-path m))
        program-str (->> (concat program ["n\n"])
                         (str/join "\n"))
        input (map int program-str)]
    (last (:output
           (intcode/run-until-halt
            (assoc (intcode/read-input-program) 0 2)
            input)))))

(comment
  (println (calc-map-str))
  (part-1 test-map)
  (part-1 input-map)
  (part-2 test-map)
  (part-2 test-map-2)
  (part-2 input-map))