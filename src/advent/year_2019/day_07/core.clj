(ns advent.year-2019.day-07.core
  (:require [advent.year-2019.intcode :as intcode]))

(defn run-setting-part-1 [program phases]
  (loop [input 0
         phases phases]
    (let [output (-> (intcode/run-until-halt program [(first phases) input])
                     :output
                     first)]
      (if-let [phases (next phases)]
        (recur output phases)
        output))))

(defn permutations [xs]
  (if (empty? xs)
    [[]]
    (->> (range (count xs))
         (mapcat (fn [n]
                   (let [[before starting-at] (split-at n xs)
                         item (first starting-at)
                         after (next starting-at)]
                     (for [perm (permutations (concat before after))]
                       (conj perm item))))))))

(defn part-1 [program]
  (->> (range 5)
       permutations
       (map #(run-setting-part-1 program %))
       (apply max)))

(defn run-until-output [state]
  (let [out-state (->> (iterate intcode/step state)
                       (take-while some?)
                       (drop-while (comp empty? :output))
                       first)]
    [(-> out-state :output first)
     (assoc out-state :output [])]))

(defn run-setting-part-2 [program phases]
  (loop [signal 0
         states (->> phases
                     (mapv #(intcode/init-state program [%])))
         current-amp 0]
    (let [[output next-state] (-> (states current-amp)
                                  (update :input conj signal)
                                  run-until-output)]
      (if output
        (recur output
               (assoc states current-amp next-state)
               (mod (inc current-amp) 5))
        (do
          (assert (= current-amp 0))
          signal)))))

(defn part-2 [program]
  (->> (range 5 10)
       permutations
       (map #(run-setting-part-2 program %))
       (apply max)))

(comment
  (part-1 (intcode/read-program "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
  (part-1 (intcode/read-input-program))
  (part-2 (intcode/read-program "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))
  (part-2 (intcode/read-input-program)))