(ns advent.year-2019.day-18.core
  (:require [clojure.data.priority-map :refer [priority-map]]
            [advent.tools :as tools]))

(defn read-input [file]
  (let [grid (tools/read-grid
              file
              (fn [ch]
                (cond
                  (= ch \.) []
                  (= ch \@) [:start]
                  :else (let [code (int ch)]
                          (cond
                            (<= (int \a) code (int \z))
                            [:key (- code (int \a))]

                            (<= (int \A) code (int \Z))
                            [:door (- code (int \A))])))))
        required-keys (->> grid
                           (map second)
                           (filter (fn [[type]]
                                     (= type :key)))
                           (map second)
                           (into #{}))
        start (->> grid
                   (some (fn [[point [type]]]
                           (when (= type :start)
                             point))))]
    {:grid grid
     :required-keys required-keys
     :start start}))

(defn neighbours [pos]
  (->> [[1 0]
        [-1 0]
        [0 1]
        [0 -1]]
       (map #(mapv + pos %))))

(def seen-fld (atom #{}))

(defn flood-search [grid pos missing-keys]
  (assert (not (@seen-fld [pos missing-keys])))
  (swap! seen-fld conj [pos missing-keys])
  (loop [steps 0
         seen #{pos}
         res []
         explore [pos]]
    (if (empty? explore)
      res
      (let [neighbs (->> explore
                         (mapcat neighbours)
                         (remove seen)
                         (filter (fn [neighb]
                                   (when-let [[type id] (grid neighb)]
                                     (or (not= type :door)
                                         (not (missing-keys id))))))
                         distinct)
            res (->> explore
                     (map (fn [pos]
                            (let [[type id] (grid pos)]
                              (when (and (= type :key)
                                         (missing-keys id))
                                [steps pos id]))))
                     (filter some?)
                     (into res))
            steps (inc steps)
            seen (into seen neighbs)
            explore neighbs]
        (recur steps
               seen
               res
               explore)))))

(defn part-1 [file]
  (let [{:keys [grid start required-keys]} (read-input file)]
    (loop [queue (priority-map {:pos start
                                :missing-keys required-keys}
                               0)
           seen #{}]
      (let [[{:keys [pos missing-keys]} steps] (peek queue)
            next-states (->> (flood-search grid pos missing-keys)
                             (map (fn [[add-steps pos key-id]]
                                    [{:pos pos
                                      :missing-keys (disj missing-keys key-id)}
                                     (+ steps add-steps)]))
                             (remove (comp seen first)))]
        (if (empty? missing-keys)
          (do
            (prn (take 50 seen))
            steps)
          (recur (into (pop queue) next-states)
                 (into seen (map first next-states))))))))

(comment
  (part-1 "test")
  (part-1 "test2")
  (part-1 "input"))