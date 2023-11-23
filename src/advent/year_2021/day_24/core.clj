(ns advent.year-2021.day-24.core
  (:require [advent.tools :as tools]
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
   :div quot
   :mod mod
   :eql (fn [x y]
          (if (= x y)
            1
            0))})

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
              res ((op-fn operator)
                   x y)]
          (recur (assoc state arg-1 res)
                 (next program)
                 inputs)))
      state)))

(defn check-model-number [program inputs]
  (let [res (run program inputs)]
    (prn inputs res)
    (zero? (:z res))))

(defn apply-next [z input [divider a b]]
  (let [cond? (= input (+ (mod z 26) a))
        z (quot z divider)]
    (if cond?
      z
      (+ (* z 26) input b))))

(defn find-solution [params part-2?]
  (let [cache (atom {})
        rec-find (fn rec-find [z params]
                   #_(prn "rec-find" z (first params))
                   (cond
                     (empty? params)
                     (if (zero? z)
                       ()
                       nil)

                     (@cache [z params])
                     (@cache [z params])

                     :else
                     (let [remaining-division (->> params
                                                   (map first)
                                                   (apply *))
                           first-params (first params)
                           reverse-fn (if part-2?
                                        identity
                                        reverse)
                           res (when (< z remaining-division)
                                 (->> (range 1 10)
                                      reverse-fn
                                      (some (fn [input]
                                              (let [z (apply-next z input first-params)]
                                                (when-let [solution (rec-find z (next params))]
                                                  (cons input solution)))))))]
                       (swap! cache assoc [z params] res)
                       res)))]
    (rec-find 0 params)))

(defn solve [part-2?]
  (let [program (read-input "input")
        params (->> program
                    (partition 18)
                    (map (fn [prog]
                           [(last (nth prog 4))
                            (last (nth prog 5))
                            (last (nth prog 15))])))]
    (apply str (find-solution params part-2?))))

(comment
  (solve false)
  (solve true))