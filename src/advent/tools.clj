(ns advent.tools
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-int [s]
  (try
    (Long/parseLong (str s))
    (catch Exception _)))

(defn read-long [s]
  (try
    (Long/parseLong (str s))
    (catch Exception _)))

(defn read-binary [s]
  (Long/parseLong s 2))

(defn read-hex [s]
  (Long/parseLong s 16))

(defn current-directory []
  (str "src/"
       (-> *ns*
           str
           (str/replace "-" "_")
           (str/replace "." "/")
           (str/replace "core" ""))))

(defn data-file [file]
  (str (current-directory) "/" file ".txt"))

(defn read-lines [file]
  (with-open [rdr (io/reader (data-file file))]
    (doall (line-seq rdr))))

(defn read-grid-from-lines
  ([lines]
   (read-grid-from-lines lines identity))
  ([lines read-char-fn]
   (->> lines
        (map-indexed (fn [y line]
                       (->> line
                            (map-indexed (fn [x ch]
                                           [[x y] ch])))))
        (apply concat)
        (map (fn [[point ch]]
               (when-let [res (read-char-fn ch)]
                 [point res])))
        (filter some?)
        (into {}))))

(defn read-grid
  ([file]
   (read-grid-from-lines (read-lines file)))
  ([file read-char-fn]
   (read-grid-from-lines (read-lines file) read-char-fn)))

(defn read-grid-dimensions [file]
  (let [lines (read-lines file)]
    {:rows (count lines)
     :cols (count (first lines))}))

(defn read-grid-with-dimensions
  ([file]
   (read-grid-with-dimensions file identity))
  ([file read-char-fn]
   (assoc (read-grid-dimensions file)
          :grid (read-grid file read-char-fn))))

(defn read-blocks [file]
  (->> (read-lines file)
       (partition-by empty?)
       (remove #{'("")})
       doall))

(defn digit? [ch]
  (<= (int \0)
      (int ch)
      (int \9)))

(defn gcd [x y]
  (loop [factor (min x y)]
    (if (and (zero? (mod x factor))
             (zero? (mod y factor)))
      factor
      (recur (dec factor)))))

(defn lcm
  ([x y]
   (/ (* x y)
      (gcd x y)))
  ([x y & more]
   (apply lcm (lcm x y) more)))

(defn value-multimap [m]
  (->> m
       (group-by val)
       (map (fn [[value entries]]
              [value (mapv key entries)]))
       (into {})))

(defn all-pairs [v]
  (let [v (vec v)
        n (count v)]
    (for [i (range n)
          j (range (inc i) n)]
      [(nth v i) (nth v j)])))
