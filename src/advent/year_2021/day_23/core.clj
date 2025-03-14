(ns advent.year-2021.day-23.core)

(def test-data {[:A 0] :A
                [:A 1] :B
                [:B 0] :D
                [:B 1] :C
                [:C 0] :C
                [:C 1] :B
                [:D 0] :A
                [:D 1] :D})

(def input-data {[:A 0] :B
                 [:A 1] :D
                 [:B 0] :A
                 [:B 1] :C
                 [:C 0] :A
                 [:C 1] :D
                 [:D 0] :C
                 [:D 1] :B})

(def ^:dynamic *part-2* false)

(defn home-count []
  (if *part-2*
    4
    2))

(defn part-2-data [data]
  (->> data
       (map (fn [[[region number] piece]]
              (if (= number 1)
                [[region 3] piece]
                [[region number] piece])))
       (into {})
       (merge {[:A 1] :D
               [:A 2] :D
               [:B 1] :B
               [:B 2] :C
               [:C 1] :A
               [:C 2] :B
               [:D 1] :C
               [:D 2] :A})))

(defn get-final-state []
  (->> [:A :B :C :D]
       (mapcat (fn [letter]
                 (->> (range (home-count))
                      (map (fn [number]
                             [[letter number] letter])))))
       (into {})))

(def home-positions
  {:A 2
   :B 4
   :C 6
   :D 8})

(def legal-hallway-points
  (->> (range 11)
       (remove (into #{} (vals home-positions)))
       (map (partial vector :H))
       vec))

(defn hallway-position [[region number]]
  (if (= region :H)
    number
    (home-positions region)))

(defn hallway-path [from-position to-position]
  (let [f (if (> to-position from-position)
            inc
            dec)]
    (concat
     (->> (iterate f from-position)
          next
          (take-while #(not= to-position %))
          (map (partial vector :H)))
     [[:H to-position]])))

(defn calc-path [[from-region from-number :as from]
                 [to-region to-number :as to]]
  (let [from-hallway (hallway-position from)
        to-hallway (hallway-position to)
        start-path (when-not (= :H from-region)
                     (concat (for [i (range (inc from-number)
                                            (home-count))]
                               [from-region i])
                             [[:H from-hallway]]))
        end-path (when-not (= :H to-region)
                   (reverse (for [i (range to-number
                                           (home-count))]
                              [to-region i])))]
    (concat start-path
            (hallway-path from-hallway to-hallway)
            end-path)))

(def step-energy
  {:A 1
   :B 10
   :C 100
   :D 1000})

(defn try-move [[state energy] from to]
  #_(prn "try" from to)
  (let [path (calc-path from to)
        piece (state from)]
    (assert piece)
    (when-not (some state path)
      [(-> state
           (dissoc from)
           (assoc to piece))
       (+ energy
          (* (count path)
             (step-energy piece)))])))

(defn home-move [[state energy]]
  (->> state
       (some (fn [[start piece]]
               (let [[start-region _] start]
                 (when (and (not= start-region piece)
                            (every? (fn [i]
                                      (let [piece-at-home (get state [piece i])]
                                        (or (nil? piece-at-home)
                                            (= piece piece-at-home))))
                                    (range (home-count))))
                   (let [goal (some (fn [i]
                                      (when (nil? (get state [piece i]))
                                        [piece i]))
                                    (range (home-count)))]
                     (try-move [state energy] start goal))))))))

(defn run-home-moves [state-energy]
  (->> (iterate home-move state-energy)
       (take-while some?)
       last))

(defn corridor-moves [state]
  (->> state
       (filter (fn [[[region number] piece]]
                 (and (not= region :H)
                      (or (not= region piece)
                          (some (fn [i]
                                  (not= piece (get state [region i])))
                                (range number))))))
       (mapcat (fn [[from _]]
                 (->> legal-hallway-points
                      (map #(try-move [state 0] from %))
                      (filter some?)
                      (map run-home-moves))))))

(defn lowest-energy [energies]
  (when-let [energies (seq (filter some? energies))]
    (apply min energies)))

(defn part-1 [state]
  (let [cache (atom {})
        final-state (get-final-state)
        rec-solve (fn rec-solve [state]
                    (if (= state final-state)
                      0
                      (if-let [res (@cache state)]
                        res
                        (let [moves (corridor-moves state)
                              res (->> moves
                                       (map (fn [[state energy]]
                                              (when-let [used (rec-solve state)]
                                                (+ energy used))))
                                       lowest-energy)]
                          (swap! cache assoc state res)
                          res))))]
    (rec-solve state)))

(defn part-2 [state]
  (binding [*part-2* true]
    (part-1 (part-2-data state))))

(comment
  (calc-path [:H 0] [:A 0])
  (binding [*part-2* true]
    (calc-path [:C 0] [:A 0]))

  (part-1 test-data)
  (part-1 input-data)
  (part-2 test-data)
  (part-2 input-data))