(ns advent.tools
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-int [s]
  (try
    (Integer/parseInt (str s))
    (catch Exception _)))

(defn read-long [s]
  (try
    (Long/parseLong (str s))
    (catch Exception _)))

(defn read-binary [s]
  (Integer/parseInt s 2))

(defn read-hex [s]
  (Integer/parseInt s 16))

(defn data-file [file]
  (let [dir (-> *ns*
                str
                (str/replace "-" "_")
                (str/replace "." "/")
                (str/replace "core" ""))]
    (str "src/" dir "/" file ".txt")))

(defn read-lines [file]
  (with-open [rdr (io/reader (data-file file))]
    (doall (line-seq rdr))))

(defn read-grid
  ([file]
   (read-grid file identity))
  ([file read-char-fn]
   (->> file
        read-lines
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

(defn read-grid-dimensions [file]
  (let [lines (read-lines file)]
    {:rows (count lines)
     :cols (count (first lines))}))

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