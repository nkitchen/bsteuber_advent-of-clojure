(ns advent.year-2023.day-20.core
  (:require [advent.tools :as tools]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tangle.core :refer [graph->dot dot->image]]))

(defn read-input [file]
  (->> file
       tools/read-lines
       (map (fn [line]
              (let [[type-name destinations] (str/split line #" -> ")
                    module-type (case (first type-name)
                                  \% :flip-flop
                                  \& :nand
                                  :broadcaster)
                    module-name (str/replace type-name #"[%&]" "")]
                [module-name {:type module-type
                              :destinations (vec (str/split destinations #", "))}])))
       (into {})))

(defn next-state [config report-pulse [state events]]
  (when-let [event (first events)]
    (let [[from to value] event
          state (report-pulse state from value)
          {:keys [type destinations]} (config to)
          module-state (state to)
          [output module-state] (case type
                                  :broadcaster [value nil]
                                  :flip-flop (when (= value false)
                                               (let [module-state (not module-state)]
                                                 [module-state module-state]))
                                  :nand (let [module-state (assoc module-state from value)
                                              output (->> module-state
                                                          vals
                                                          (some not)
                                                          boolean)]
                                          [output module-state])
                                  nil)
          emit-events (when (some? output)
                        (->> destinations
                             (map (fn [dest]
                                    [to dest output]))))
          events (concat (next events) emit-events)
          state (if (some? module-state)
                  (assoc state
                         to module-state)
                  state)]
      [state events])))

(defn init-state [config]
  (let [links-to (->> config
                      (mapcat (fn [[module-name {:keys [destinations]}]]
                                (for [dest destinations]
                                  [module-name dest])))
                      (group-by second))]
    (->> config
         (map (fn [[module-name {:keys [type]}]]
                (when (= type :nand)
                  [module-name
                   (->> (links-to module-name)
                        (map (fn [[from _]]
                               [from false]))
                        (into {}))])))
         (filter some?)
         (into {:button-presses 0}))))

(defn push-button [config report-pulse state]
  (->> (iterate (partial next-state config report-pulse)
                [(update state :button-presses inc)
                 [["button" "broadcaster" false]]])
       (take-while some?)
       last
       first))

(defn part-1 [file]
  (let [config (read-input file)
        state (init-state config)
        report-pulse (fn [state _ value]
                       (let [k (if value
                                 :high-pulses
                                 :low-pulses)]
                         (update state k (fnil inc 0))))
        {:keys [high-pulses
                low-pulses]} (->> state
                                  (iterate (partial push-button
                                                    config report-pulse))
                                  (drop 1000)
                                  first)]
    (* low-pulses high-pulses)))

(defn visualize-config [config]
  (let [as-node (fn [n]
                  (str n " " (case (get-in config [n :type])
                               :flip-flop "%"
                               :nand "&"
                               "")))
        nodes (->> config
                   keys
                   (map as-node))
        edges (->> config
                   (mapcat (fn [[from {:keys [destinations]}]]
                             (->> destinations
                                  (map (fn [to]
                                         [(as-node from)
                                          (as-node to)
                                          {:arrowType :normal
                                           :dir :forward}]))))))
        dot (graph->dot nodes edges
                        {:node {:shape :box}
                         :node->id identity
                         :node->descriptor (constantly nil)})]
    (io/copy (dot->image dot "png")
             (io/file (str (tools/current-directory)
                           "/graph.png")))))

(defn find-links-to [config destination]
  (->> config
       (map (fn [[module-name {:keys [destinations]}]]
              (when (some #{destination} destinations)
                module-name)))
       (filter some?)))

(defn find-output-modules [config]
  (->> (find-links-to config "rx")
       (mapcat #(find-links-to config %))))

(defn part-2 [file]
  (let [config (read-input file)
        state (init-state config)
        output-module? (into #{} (find-output-modules config))
        report-pulse (fn [state from value]
                       (if (and value
                                (output-module? from))
                         (update state :history
                                 (fnil conj [])
                                 [from (:button-presses state)])
                         state))]
    (->> state
         (iterate (partial push-button config report-pulse))
         (drop (Math/pow 2 12))
         first
         :history
         (map second)
         (apply *))))

(comment
  (part-1 "test")
  (part-1 "test2")
  (part-1 "input")
  (visualize-config (read-input "input"))
  (part-2 "input"))


