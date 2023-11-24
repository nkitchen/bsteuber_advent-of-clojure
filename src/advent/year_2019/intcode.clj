(ns advent.year-2019.intcode
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

; Days: 2 5 7 9

(defn read-program [s]
  (->> (str/split s #",")
       (mapv #(Long/parseLong %))))

(defn read-input-program []
  (->> "input"
       tools/read-lines
       first
       read-program))

(defn fetch-args [{:keys [program position relative-base]}
                  arg-specs flags]
  (->> (map vector arg-specs (concat flags [\0 \0 \0 \0]))
       (map-indexed (fn [i [arg-spec flag]]
                      (let [arg (get program (+ position i 1) 0)
                            arg (if (= flag \2)
                                  (+ relative-base arg)
                                  arg)]
                        (if (and (= arg-spec :in)
                                 (not= flag \1))
                          (get program arg 0)
                          arg))))
       vec))

(defn binary-op [op-fn state flags]
  (let [[x y result-pos] (fetch-args state [:in :in :out] flags)
        result (op-fn x y)]
    (-> state
        (update :position + 4)
        (update :program assoc result-pos result))))

(defn fetch-input [state flags]
  (let [[result-pos] (fetch-args state [:out] flags)
        input (-> state :input first)]
    (-> state
        (update :position + 2)
        (update :program assoc result-pos input)
        (update :input next))))

(defn write-output [state flags]
  (let [[x] (fetch-args state [:in] flags)]
    (-> state
        (update :position + 2)
        (update :output conj x))))

(defn jump-if [value state flags]
  (let [[test-arg target-arg] (fetch-args state [:in :in] flags)
        non-zero? (not (zero? test-arg))
        condition (= value non-zero?)]
    (if condition
      (assoc state :position target-arg)
      (update state :position + 3))))

(defn predicate [op]
  (fn [x y]
    (if (op x y)
      1
      0)))

(defn adjust-relative-base [state flags]
  (let [[x] (fetch-args state [:in] flags)]
    (-> state
        (update :relative-base + x)
        (update :position + 2))))

(defn step [{:keys [position program]
             :as state}]
  (let [op-code-with-flags (get program position)
        op-code (mod op-code-with-flags 100)
        flags (->> (quot op-code-with-flags 100)
                   str
                   reverse)]
    (case op-code
      99 nil
      1 (binary-op + state flags)
      2 (binary-op * state flags)
      3 (fetch-input state flags)
      4 (write-output state flags)
      5 (jump-if true state flags)
      6 (jump-if false state flags)
      7 (binary-op (predicate <) state flags)
      8 (binary-op (predicate =) state flags)
      9 (adjust-relative-base state flags))))

(defn init-state [program input]
  {:program (->> program
                 (map-indexed vector)
                 (into {}))
   :input input
   :output []
   :position 0
   :relative-base 0})

(defn run-states [state]
  (->> state
       (iterate step)
       (take-while some?)))

(defn run-until-halt [program input]
  (-> (init-state program input)
      run-states
      last))

(defn next-output [state input]
  (let [state (->> (assoc state :input input)
                   run-states
                   (drop-while (comp empty? :output))
                   first)]
    [(first (:output state))
     (assoc state :output [])]))