(ns advent.year-2024.day-21.core
  (:require
   [advent.grid :as grid]
   [advent.tools :as tools]))

(def numeric-keypad
  {\7 [0 0]
   \8 [1 0]
   \9 [2 0]
   \4 [0 1]
   \5 [1 1]
   \6 [2 1]
   \1 [0 2]
   \2 [1 2]
   \3 [2 2]
   \0 [1 3]
   \A [2 3]})

(def directional-keypad
  {\^ [1 0]
   \A [2 0]
   \< [0 1]
   \v [1 1]
   \> [2 1]})

(defn inverse-map [m]
  (->> m
       (map (fn [[k v]]
              [v k]))
       (into {})))

(def numeric-grid (inverse-map numeric-keypad))
(def directional-grid (inverse-map directional-keypad))

(defn possible-paths [keypad grid from to]
  (let [start-point (keypad from)
        end-point (keypad to)]
    (loop [explore [start-point]
           known-paths {start-point [""]}]
      (when (empty? explore)
        (throw (ex-info (str "No path found"
                             {:grid grid
                              :from from
                              :to to})
                        {})))
      (if-let [paths (known-paths end-point)]
        (mapv #(str % \A) paths)
        (let [todo (->> explore
                        (mapcat (fn [point]
                                  (let [paths (known-paths point)]
                                    (->> grid/directions-4
                                         (map (fn [dir]
                                                (let [neighb (grid/go-dir point dir)]
                                                  (when (and (grid neighb)
                                                             (not (known-paths neighb)))
                                                    [neighb (mapv #(str % (grid/format-direction dir))
                                                                  paths)]))))
                                         (filter some?))))))
              explore (->> todo
                           (map first)
                           distinct)
              known-paths (reduce (fn [known-paths [point paths]]
                                    (update known-paths point (fnil into []) paths))
                                  known-paths
                                  todo)]
          (recur explore known-paths))))))

(def numeric-paths (memoize (partial possible-paths numeric-keypad numeric-grid)))
(def directional-paths (memoize (partial possible-paths directional-keypad directional-grid)))

(def paths-cache (atom {}))

(declare best-path-length)

(defn best-word-length [level s]
  (loop [[next-key & more-keys] s
         current-key \A
         length 0]
    (if-not next-key
      length
      (recur more-keys
             next-key
             (+' length
                 (best-path-length level current-key next-key))))))

(defn best-path-length [level from to]
  (or (@paths-cache [level from to])
      (let [paths (if (and (numeric-keypad from)
                           (numeric-keypad to))
                    (numeric-paths from to)
                    (directional-paths from to))
            res (if (zero? level)
                  (count (first paths))
                  (->> paths
                       (map (fn [path]
                              (best-word-length (dec level) path)))
                       (apply min)))]
        (swap! paths-cache assoc [level from to] res)
        res)))

(defn run [directional-robots file]
  (reset! paths-cache {})
  (->> file
       tools/read-lines
       (map (fn [line]
              (*' (best-word-length directional-robots line)
                  (tools/read-long (re-find #"\d+" line)))))
       (apply +')))

(def part-1 (partial run 2))
(def part-2 (partial run 25))

(comment
  (directional-paths \< \^)
  (best-word-length 0 "029A")
  (part-1 "test")
  (part-1 "input")
  (part-2 "input"))
