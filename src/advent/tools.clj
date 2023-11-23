(ns advent.tools
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-int [s]
  (Integer/parseInt (str s)))

(defn read-long [s]
  (Long/parseLong (str s)))

(defn read-binary [s]
  (Integer/parseInt s 2))

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

(defn read-blocks [file]
  (->> (read-lines file)
       (partition-by empty?)
       (remove #{'("")})
       doall))

