(ns advent.year-2023.day-08.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input [file]
  (let [[[directions] nodes] (tools/read-blocks file)]
    {:directions directions
     :nodes (->> nodes
                 (map (fn [line]
                        (let [[node-name children] (str/split line #" = ")
                              [left right] (-> children
                                               (str/replace #"[()]" "")
                                               (str/split #", "))]
                          [node-name [left right]])))
                 (into {}))}))

(defn step [nodes current-node dir]
  (let [[left right] (get nodes current-node)]
    (case dir
      \L left
      \R right)))

(defn path-from [{:keys [directions nodes]} start-node]
  (->> directions
       cycle
       (reductions (partial step nodes) start-node)
       (map-indexed vector)))

(defn part-1 [file]
  (let [data (read-input file)]
    (->> (path-from data "AAA")
         (some (fn [[steps node]]
                 (when (= node "ZZZ")
                   steps))))))

(defn calc-cycle-data [data end-node?-fn start-node]
  (let [num-directions (count (:directions data))]
    (loop [path (path-from data start-node)
           end-node-ids []
           seen {}]
      (let [[i node] (first path)
            dir-index (mod i num-directions)
            lookup-key [node dir-index]]
        (if-let [seen-index (seen lookup-key)]
          [seen-index (- i seen-index) (mapv #(- % seen-index)
                                             end-node-ids)]
          (let [end-node-ids (if (end-node?-fn node)
                               (conj end-node-ids i)
                               end-node-ids)]
            (recur (next path)
                   end-node-ids
                   (assoc seen lookup-key i))))))))

(defn combine [[cycle-start-1 cycle-len-1 end-node-ids-1]
               [cycle-start-2 cycle-len-2 end-node-ids-2]]
  (let [cycle-start (max cycle-start-1 cycle-start-2)
        cycle-len (tools/lcm cycle-len-1 cycle-len-2)
        offset-1 (- cycle-start cycle-start-1)
        offset-2 (- cycle-start cycle-start-2)
        end-node-ids-1 (map #(- % offset-1) end-node-ids-1)
        end-node-ids-2 (map #(- % offset-2) end-node-ids-2)
        all-end-node-ids-1 (->> (range (/ cycle-len cycle-len-1))
                                (mapcat (fn [cycle-index]
                                          (let [delta (* cycle-index cycle-len-1)]
                                            (map #(+ % delta)
                                                 end-node-ids-1))))
                                (into #{}))
        all-end-node-ids-2 (->> (range (/ cycle-len cycle-len-2))
                                (mapcat (fn [cycle-index]
                                          (let [delta (* cycle-index cycle-len-2)]
                                            (map #(+ % delta)
                                                 end-node-ids-2))))
                                (into #{}))
        end-node-ids (set/intersection all-end-node-ids-1
                                       all-end-node-ids-2)]
    [cycle-start cycle-len end-node-ids]))

(defn part-2 [file]
  (let [data (read-input file)
        all-nodes (keys (:nodes data))
        start-nodes (->> all-nodes
                         (filter #(str/ends-with? % "A")))
        end-node? (->> all-nodes
                       (filter #(str/ends-with? % "Z"))
                       (into #{}))
        all-cycle-data (->> start-nodes
                            (mapv #(calc-cycle-data data end-node? %)))
        ;[offset _ end-node-ids] (reduce combine (take 5 all-cycle-data))
        ]
    (->> all-cycle-data
         (map second)
         (reduce tools/lcm))))

(comment
  (part-1 "test")
  (part-1 "test2")
  (part-1 "input")
  (combine [1 2 [1]] [1 6 [2 5]])
  (part-2 "test3")
  (part-2 "input"))