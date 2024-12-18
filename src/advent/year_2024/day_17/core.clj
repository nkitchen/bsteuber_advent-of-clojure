(ns advent.year-2024.day-17.core
  (:require
   [advent.tools :as tools]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn read-input [file]
  (let [[reg-a reg-b reg-c & program]
        (->> file
             tools/read-lines
             (mapcat (fn [line]
                       (re-seq #"\d+" line)))
             (map tools/read-long))]
    {:register {:a reg-a
                :b reg-b
                :c reg-c}
     :program (vec program)}))

(defn goto-next-instruction [state]
  (update state :instruction + 2))

(defn literal [arg _]
  arg)

(defn combo [arg {:keys [register]}]
  (cond
    (<= 0 arg 3) arg
    (= arg 4) (:a register)
    (= arg 5) (:b register)
    (= arg 6) (:c register)
    :else (throw (ex-info "Illegal combo arg" {:arg arg}))))

(defn fn-op [op-fn in-reg read-arg-fn out-reg]
  (fn [arg state]
    (let [x (get-in state [:register in-reg])
          y (read-arg-fn arg state)
          res (op-fn x y)]
      (-> state
          (assoc-in [:register out-reg] res)
          goto-next-instruction))))
(defn jump-a-not-zero [arg state]
  (if (zero? (get-in state [:register :a]))
    (goto-next-instruction state)
    (assoc state :instruction (literal arg state))))

(defn write-output [arg state]
  (-> state
      (update :output conj (bit-and (combo arg state)
                                    7))
      goto-next-instruction))

(def operators
  [(fn-op bit-shift-right :a combo :a)
   (fn-op bit-xor :b literal :b)
   (fn-op (fn [_ x] (bit-and x 7)) nil combo :b)
   jump-a-not-zero
   (fn-op bit-xor :b
          (fn [_ state]
            (get-in state [:register :c]))
          :b)
   write-output
   (fn-op bit-shift-right :a combo :b)
   (fn-op bit-shift-right :a combo :c)])

(defn run-command [{:keys [instruction program]
                    :as state}]
  (let [op-code (get program instruction)
        arg (get program (inc instruction))]
    (when (and op-code arg)
      (let [op (operators op-code)]
        (op arg state)))))

(defn run
  [state]
  (let [init-state (assoc state
                          :instruction 0
                          :output [])]
    (->> (iterate run-command init-state)
         (take-while some?)
         last)))

(defn get-output [{:keys [output]}]
  (->> output
       (str/join ",")))

(defn part-1 [file]
  (-> file
      read-input
      run
      get-output))

; Input program analysis:
; 2,4,
; 0: b = a mod 8
; 1,2,
; 2: b = b xor 2
; 7,5,
; 4: c = a >> b
; 4,1,
; 6: b = b xor c
; 1,3
; 8: b = b xor 3
; 5,5,
; 10: print b mod 8
; 0,3,
; 12: a = a >> 3
; 3,0
; 14: if a == 0 then done else goto 0

(defn part-2 [file]
  (let [state (read-input file)
        {:keys [program]} state
        try-value (fn [reg-a target-output]
                    (let [output (-> state
                                     (assoc-in [:register :a] reg-a)
                                     run
                                     :output)]
                      (= target-output output)))]
    (loop [solutions [0]
           match-outputs 1]
      (if (> match-outputs (count program))
        (apply min solutions)
        (let [target-output (take-last match-outputs program)
              next-solutions (->> (for [sol solutions
                                        num (range 8)]
                                    (+ num (bit-shift-left sol 3)))
                                  (filterv (fn [x]
                                             (try-value x target-output))))]
          (recur next-solutions
                 (inc match-outputs)))))))

(deftest run-test-1
  (let [state (run {:register {:c 9}
                    :program [2 6]})]
    (is (= (get-in state [:register :b])
           1))))

(deftest run-test-2
  (let [state (run {:register {:a 10}
                    :program [5 0 5 1 5 4]})]
    (is (= (:output state)
           [0 1 2]))))

(deftest run-test-3
  (let [state (run {:register {:a 2024}
                    :program [0 1 5 4 3 0]})]
    (is (= (:output state)
           [4,2,5,6,7,7,7,7,3,1,0]))
    (is (= (get-in state [:register :a])
           0))))

(deftest run-test-4
  (let [state (run {:register {:b 29}
                    :program [1 7]})]
    (is (= (get-in state [:register :b])
           26))))

(deftest run-test-5
  (let [state (run {:register {:b 2024
                               :c 43690}
                    :program [4 0]})]
    (is (= (get-in state [:register :b])
           44354))))

(comment
  (part-1 "test1")
  (part-1 "input")
  (part-2 "test2")
  (part-2 "input"))
