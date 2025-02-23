(ns advent.year-2024.day-24.core
  (:require
   [advent.tools :as tools]
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [clojure.set :as set]))

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
        imax (apply max (->> (range)
                               (take-while #(initially-known (wire-name "x" %)))))
        n (inc imax)
        gate-of (into {} (for [[output [a op b]] rules]
                           [output [op #{a b}]]))
        gate2-out (set/map-invert gate-of)
        gate1-out (into {} (for [[output [op inputs]] gate-of
                                 in inputs]
                             [[op in] output]))]
    (loop [i 1
           cin "drq" ; carry in
           ]
      (let [[sigs miswired]
            (reduce
              (fn [[sigs miswired] [out op in1 in2]]
                (let [in1-sig (sigs in1)
                      in2-sig (sigs in2)
                      out-sig (gate2-out [op #{in1-sig in2-sig}])
                      out-sig1 (gate1-out [op in1-sig])
                      out-sig2 (gate1-out [op in2-sig])]
                  (cond
                    out-sig [(assoc sigs out out-sig) miswired]
                    out-sig1 [(assoc sigs out out-sig1) (conj miswired [in2-sig :not op in1-sig in2-sig])]
                    out-sig2 [(assoc sigs out out-sig2) (conj miswired [in1-sig :not op in1-sig in2-sig])]
                    :else (assert false [i sigs]))))

              [{:x (wire-name "x" i)
                :y (wire-name "y" i)
                :cin cin}
               #{}]

              [[:s1 "XOR" :x :y]
               [:cg "AND" :x :y]
               [:s2 "XOR" :s1 :cin]
               [:cp "AND" :cin :s1]
               [:co "OR" :cg :cp]])]

        (if (seq miswired)
          (println "Miswired: " miswired))
        (if (< i 44)
          (recur (inc i)
                 (sigs :co)))))))

; (comment
(doseq [r [
  (format "%02d" 3)
  (time (part-1 "test1"))
  (time (part-1 "test2"))
  (time (part-1 "input"))
  ;(time (part-2 "test3"))
  (time (part-2 "input"))
  ]]
  (println r))
