(ns advent.year-2019.day-12.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (-> line
                   (str/replace #"[<>=,xyz]" "")
                   (str/split #" ")
                   (->> (mapv tools/read-int)))))))

(defn step [[positions velocities]]
  (let [velocities (->> positions
                        (map-indexed
                         (fn [i pos]
                           (->> positions
                                (map-indexed
                                 (fn [j other-pos]
                                   (when-not (= i j)
                                     (mapv (fn [coord other-coord]
                                             (cond
                                               (> other-coord coord) 1
                                               (< other-coord coord)  -1
                                               :else 0))
                                           pos other-pos))))
                                (filter some?)
                                (apply mapv + (velocities i)))))
                        vec)
        positions (mapv (fn [pos vel]
                          (mapv + pos vel))
                        positions
                        velocities)]
    [positions velocities]))

(defn single-energy [v]
  (->> v
       (map #(Math/abs %))
       (apply +)))

(defn calc-energy [state]
  (->> state
       (apply map (fn [pos vel]
                    (* (single-energy pos)
                       (single-energy vel))))
       (apply +)))

(defn part-1 [file steps]
  (let [positions (read-input file)
        velocities (vec (repeat 4
                                (vec (repeat 3 0))))
        init-state [positions velocities]
        end-state (nth (iterate step init-state)
                       steps)]
    (calc-energy end-state)))

(defn gcd [x y]
  (loop [factor (min x y)]
    (if (and (zero? (mod x factor))
             (zero? (mod y factor)))
      factor
      (recur (dec factor)))))

(defn lcm
  ([x y]
   (/ (* x y)
      (gcd x y)))
  ([x y & more]
   (apply lcm (lcm x y) more)))

(defn part-2 [file]
  (let [positions (read-input file)
        velocities (vec (repeat 4
                                (vec (repeat 3 0))))
        init-states (for [i (range 3)]
                      (->> [positions velocities]
                           (mapv (fn [moons]
                                   (->> moons
                                        (mapv #(vector (get % i))))))))
        cycles (->> init-states
                    (mapv (fn [init-state]
                            (->> (iterate step init-state)
                                 (map-indexed vector)
                                 next
                                 (take 1000000)
                                 (some (fn [[i state]]
                                         (when (= state init-state)
                                           i)))))))]
    (apply lcm cycles)))

(comment
  (part-1 "test" 10)
  (part-1 "test2" 100)
  (part-1 "input" 1000)
  (part-2 "test")
  (part-2 "test2")
  (part-2 "input"))