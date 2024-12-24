(ns advent.year-2024.day-24.core
  (:require
   [advent.tools :as tools]
   [clojure.string :as str]))

(defn read-input [file]
  (let [[known-lines rule-lines] (tools/read-blocks file)
        known (->> known-lines
                   (map (fn [line]
                          (let [[wire value] (str/split line #": ")]
                            [wire (tools/read-int value)])))
                   (into {}))
        rules (->> rule-lines
                   (map (fn [line]
                          (let [[input-1 op input-2 _-> output] (str/split line #" ")]
                            [output [input-1 op input-2]])))
                   (into {}))]
    [known rules]))

(defn evaluator [initially-known rules]
  (let [known-state (atom initially-known)
        rec-eval (fn rec-eval [wire]
                   (or (@known-state wire)
                       (let [[input-1 op input-2] (rules wire)
                             value-1 (rec-eval input-1)
                             value-2 (rec-eval input-2)
                             op-fn (case op
                                     "AND" bit-and
                                     "OR" bit-or
                                     "XOR" bit-xor)
                             output (op-fn value-1 value-2)]
                         (swap! known-state assoc wire output)
                         output)))]
    rec-eval))

(defn part-1 [file]
  (let [[initially-known rules] (read-input file)
        eval-fn (evaluator initially-known rules)]
    (->> rules
         keys
         (filter #(str/starts-with? % "z"))
         sort
         reverse
         (map eval-fn)
         str/join
         tools/read-binary)))

(defn wire-name [prefix index]
  (format "%s%02d" prefix index))

(defn part-2 [file]
  (let [[initially-known rules] (read-input file)
        zero-inputs (->> initially-known
                         (map (fn [[input _]]
                                [input 0]))
                         (into {}))]
    (->> (range)
         (map (fn [i]
                (let [output (wire-name "z" i)
                      input-1 (wire-name "x" i)
                      input-2 (wire-name "y" i)
                      check (fn [x y z]
                              (= z ((evaluator (assoc zero-inputs
                                                      input-1 x
                                                      input-2 y)
                                               rules)
                                    output)))]
                  (when (rules output)
                    [output (and
                             (check 0 0 0)
                             (check 0 1 1)
                             (check 1 0 1)
                             (check 1 1 0))]))))
         (take-while some?))))

(comment
  (format "%02d" 3)
  (part-1 "test1")
  (part-1 "test2")
  (part-1 "input")
  (part-2 "test3")
  (part-2 "input"))
