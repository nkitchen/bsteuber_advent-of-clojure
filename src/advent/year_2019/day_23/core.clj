(ns advent.year-2019.day-23.core
  (:require [advent.year-2019.intcode :as intcode]))

(defn setup-machines []
  (let [program (intcode/read-input-program)]
    (->> (range 50)
         (mapv (fn [address]
                 (intcode/init-state program (cons address (repeat -1))))))))

(defn add-to-queue [machine data]
  (let [queue (take-while #(not= -1 %) (:input machine))]
    (assoc machine
           :input (concat queue data (repeat -1)))))

(defn fresh-output [{:keys [output]}]
  (when (= 3 (count output))
    output))

(defn step [state]
  (let [{:keys [machines nat idle-steps]
         :or {idle-steps 0}} state
        idle? (every? (fn [machine]
                        (= -1 (first (:input machine))))
                      machines)
        idle-steps (if idle?
                     (inc idle-steps)
                     0)
        [machines sent-nat] (if (> idle-steps 100)
                              [(update machines 0 add-to-queue nat) nat]
                              [machines nil])
        machines (->> machines
                      (mapv intcode/step))
        packets (->> machines
                     (map fresh-output)
                     (filterv some?))
        machines (->> machines
                      (mapv (fn [machine]
                              (if (fresh-output machine)
                                (assoc machine :output [])
                                machine))))
        [machines nat] (reduce (fn [[machines nat] [address x y]]
                                 (if (= address 255)
                                   [machines [x y]]
                                   [(update machines address add-to-queue [x y])
                                    nat]))
                               [machines nat]
                               packets)]
    {:machines machines
     :nat nat
     :packets packets
     :idle-steps idle-steps
     :sent-nat sent-nat}))

(defn part-1 []
  (->> (iterate step {:machines (setup-machines)})
       (take 10000)
       (map :packets)
       (some (fn [packets]
               (some (fn [[address _ y]]
                       (when (= address 255)
                         y))
                     packets)))))

(defn part-2 []
  (->> (iterate step {:machines (setup-machines)})
       (take 1000000)
       (map :sent-nat)
       (filter some?)
       (map second)
       (partition 2 1)
       (some (fn [[y1 y2]]
               (when (= y1 y2)
                 y1)))))

(comment
  (part-1)
  (part-2))