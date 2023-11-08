(ns advent.year-2021.day-21.core
  (:require [clojure.string :as str]))

(def test-data [4 8])

(def input-data [3 5])

(defn step-forward [field steps]
  (-> field
      dec
      (+ steps)
      (mod 10)
      inc))

(defn part-1 [[field-1 field-2]]
  (loop [dice-rolled 0
         field-1 field-1
         score-1 0
         field-2 field-2
         score-2 0]
    (let [move-fields (+ (* 3 dice-rolled) 6)
          dice-rolled (+ dice-rolled 3)
          field-1 (step-forward field-1 move-fields)
          score-1 (+ score-1 field-1)]
      (if (>= score-1 1000)
        (* dice-rolled score-2)
        (recur dice-rolled
               field-2
               score-2
               field-1
               score-1)))))

(defn flip [[x y]]
  [y x])

(defn pr-depth [depth]
  (str/join (repeat depth " ")))

(defn part-2 [[field-1 field-2]]
  (let [cache (atom {})
        rec-solve (fn rec-solve [state depth]
                    (if-let [res (@cache state)]
                      (do
                        ;(println (pr-depth depth) "cached" state "=>" res)
                        res)
                      (let [[field-1 score-1 field-2 score-2] state
                            ;_ (println (pr-depth depth) "rec-solve" state)
                            res (->> [1 2 3]
                                     (map (fn [move-steps]
                                            ;(println (pr-depth depth) move-steps)
                                            (let [field-1 (step-forward field-1 move-steps)
                                                  score-1 (+ score-1 field-1)]
                                              (if (>= score-1 21)
                                                [1 0]
                                                (flip (rec-solve [field-2 score-2 field-1 score-1] (inc depth)))))))
                                     (apply map +))
                            #_(println (pr-depth depth) "rec-solve" state "=>" res)]
                        (swap! cache assoc state res)
                        res)))]
    (rec-solve [field-1 0 field-2 0] 0)))

(defn part-2 [[field-1 field-2]]
  (let [cache (atom {})
        rec-solve (fn rec-solve [state]
                    (if-let [res (@cache state)]
                      res
                      (let [[field-1 score-1 field-2 score-2] state
                            res (->> (for [x [1 2 3]
                                           y [1 2 3]
                                           z [1 2 3]]
                                       (+ x y z))
                                     (map (fn [move-steps]
                                            (let [field-1 (step-forward field-1 move-steps)
                                                  score-1 (+ score-1 field-1)]
                                              (if (>= score-1 21)
                                                [1 0]
                                                (flip (rec-solve [field-2 score-2 field-1 score-1]))))))
                                     (apply map +))]
                        (swap! cache assoc state res)
                        res)))]
    (apply max (rec-solve [field-1 0 field-2 0]))))

(comment
  (part-1 test-data)
  (part-1 input-data)
  (part-2 test-data)
  (part-2 input-data))