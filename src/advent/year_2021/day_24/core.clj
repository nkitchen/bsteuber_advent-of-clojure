(ns advent.year-2021.day-24.core
  (:require [advent.tools :as tools]
            [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (->> (str/split line #" ")
                    (mapv (fn [token]
                            (if (re-find #"[0-9]" token)
                              (tools/read-int token)
                              (keyword token)))))))))

(def op-fn
  {:add +
   :mul *
   :div (fn [x y]
          (int (Math/floor (/ x y))))
   :mod mod
   :eql (fn [x y]
          (if (= x y)
            1
            0))})

(defn enumerate-inputs [input-ids]
  (let [rec-fn (fn rec-fn [input-ids]
                 (if-let [input-id (first input-ids)]
                   (for [kv-pairs (rec-fn (next input-ids))
                         value (range 1 10)]
                     (concat [[input-id value]] kv-pairs))
                   [[]]))]

    (mapv #(into {} %) (rec-fn input-ids))))

(defn eval-expr [expr inputs]
  (let [res
        (if (number? expr)
          expr
          (let [[op & args] expr]
            (if (= op :i)
              (inputs (first args))
              (apply (op-fn op)
                     (map #(eval-expr % inputs) args)))))]
    res))

(declare simplify)

(defn simplify-op [expr]
  (match expr
    [:add 0 x] x
    [:add x 0] x
    [:add
     (n :guard number?)
     (m :guard number?)] (+ n m)
    [:add
     [:add x (n :guard number?)]
     (m :guard number?)] (simplify [:add x (+ n m)])
    [:add
     [:add x (n :guard number?)]
     [:add y (m :guard number?)]] [:add [:add x y] (+ n m)]
    [:mul 1 x] x
    [:mul x 1] x
    [:div x 1] x
    [:div 0 _] 0
    [:mul [:add x y] z] [:add
                         (simplify [:mul x z])
                         (simplify [:mul y z])]
    [:mul [:mul
           x
           (y :guard number?)]
     (z :guard number?)] [:mul x (* y z)]
    [:mod [:add x y] z] (simplify [:add
                                   (simplify [:mod x z])
                                   (simplify [:mod y z])])
    :else nil))

(defn simplify-invariant [expr]
  (let [inputs (->> expr
                    flatten
                    (partition 2 1)
                    (filter (comp #{:i} first))
                    (map second)
                    distinct)]
    (when (< (count inputs) 5)
      (let [results (->> (enumerate-inputs inputs)
                         (map #(eval-expr expr %))
                         distinct)]
        (when (= 1 (count results))
          (first results))))))

(simplify [:add 0 0])

(defn simplify [expr]
  (or (simplify-op expr)
      (simplify-invariant expr)
      expr))

(defn run [program inputs]
  (loop [state {:w 0 :x 0 :y 0 :z 0}
         program program
         inputs inputs]
    (if-let [[operator arg-1 arg-2] (first program)]
      (if (= operator :inp)
        (if-let [input (first inputs)]
          (recur (assoc state arg-1 input)
                 (next program)
                 (next inputs))
          (do
            (println "Out of inputs")
            state))
        (let [x (get state arg-1)
              y (if (number? arg-2)
                  arg-2
                  (get state arg-2))
              res (if (or (vector? x)
                          (vector? y))
                    (simplify [operator x y])
                    ((op-fn operator)
                     x y))]
          (recur (assoc state arg-1 res)
                 (next program)
                 inputs)))
      state)))

(defn check-model-number [program inputs]
  (let [res (run program inputs)]
    (prn inputs res)
    (zero? (:z res))))

(defn part-1 []
  (let [program (read-input "input")]
    #_(doseq [d0 (range 1 10)
            ;d1 (range 1 10)
              ]
        (run program [d0 #_d1]))
    (pprint (:z
             (run program (for [input (range 6)]
                            [:i input]))))))

(part-1)

(comment

  (run (read-input "test") 10)
  (enumerate-inputs [0 1]))