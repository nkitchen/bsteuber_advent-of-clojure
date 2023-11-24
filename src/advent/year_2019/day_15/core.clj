(ns advent.year-2019.day-15.core
  (:require [advent.year-2019.intcode :as intcode]))

(def movements
  {1 [0 -1]
   2 [0 1]
   3 [-1 0]
   4 [1 0]})

(defn flood-step [{:keys [seen explore]
                   :as data}]
  (when-let [[position state] (first explore)]
    (let [data (-> data
                   (update :explore dissoc position))]
      (reduce
       (fn [data [dir-cmd delta]]
         (let [next-pos (mapv + position delta)]
           (if (seen next-pos)
             data
             (let [[output state] (intcode/next-output state [dir-cmd])
                   state (update state :steps inc)]
               (cond-> data
                 true
                 (assoc-in [:seen next-pos] output)

                 (not= output 0)
                 (assoc-in [:explore next-pos] state)

                 (= output 2)
                 (assoc :oxigen-pos next-pos
                        :oxigen-steps (:steps state)))))))
       data
       movements))))

(defn explore []
  (let [program (intcode/read-input-program)
        init-state (intcode/init-state program [])]
    (select-keys
     (->> {:seen {[0 0] 1}
           :explore {[0 0] (assoc init-state
                                  :steps 0)}}
          (iterate flood-step)
          (take-while some?)
          last)
     [:seen :oxigen-pos :oxigen-steps])))

(defn part-1 []
  (:oxigen-steps (explore)))

(defn part-2 []
  (let [{:keys [seen oxigen-pos]} (explore)]
    (loop [steps 0
           left (->> seen
                     (filter (comp #{1} val))
                     (map first)
                     (into #{}))
           explore [oxigen-pos]]
      (if (empty? left)
        steps
        (let [left-neighbs (->> explore
                                (mapcat (fn [pos]
                                          (->> (vals movements)
                                               (map #(mapv + pos %))
                                               (filter left))))
                                distinct)]
          (recur (inc steps)
                 (apply disj left left-neighbs)
                 left-neighbs))))))

(comment
  (part-1)
  (part-2))