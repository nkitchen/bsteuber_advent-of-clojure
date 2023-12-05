(ns advent.year-2019.day-25.core
  (:require [advent.year-2019.intcode :as intcode]
            [clojure.string :as str]
            [advent.tools :as tools]
            [clojure.set :as set]))

(defn read-output [state]
  (->> state
       :output
       (map char)
       (apply str)
       str/split-lines
       (remove empty?)))

(defn run-until-input [state]
  (let [last-output (atom nil)]
    (or
     (->> state
          intcode/run-states
          (some (fn [state]
                  (let [output (read-output state)]
                    (reset! last-output output)
                    (when (= (last output) "Command?")
                      (assoc state :game-output (butlast output)))))))
     (do
       (println "Game over...")
       (doseq [line @last-output]
         (println line))))))

(defn start []
  (-> (intcode/read-input-program)
      (intcode/init-state [])
      run-until-input))

(defn command [state command]
  (if state
    (-> state
        (assoc :input (->> (str command "\n")
                           (mapv int))
               :output [])
        run-until-input)
    (throw (Exception. (str "Empty state before command: " command)))))

(defn read-room-name [[room-line & more-lines :as lines]]
  (if (and (str/starts-with? room-line "== ")
           (str/ends-with? room-line " =="))
    [(->> room-line
          (drop 3)
          (drop-last 3)
          (apply str))
     more-lines]
    lines))

(def list-name-mapping
  {"Doors here lead" :doors
   "Items here" :items})

(defn read-room-lists [lines]
  (loop [data {}
         lines lines]
    (let [list-name (first lines)]
      (if (and list-name (str/ends-with? list-name ":"))
        (let [list-name (apply str (butlast list-name))
              list-name (or (list-name-mapping list-name)
                            (throw (Exception. (str "Unknown list name: " list-name))))
              [elements lines] (->> lines
                                    next
                                    (split-with #(str/starts-with? % "- ")))]
          (recur (assoc data list-name (mapv #(subs % 2) elements))
                 lines))
        data))))

(defn read-room-data [{:keys [game-output]}]
  (let [[room-name lines] (read-room-name game-output)
        [text & lines] lines]
    (merge {:room-name room-name
            :text text}
           (read-room-lists lines))))

(defn explore-rooms []
  (loop [by-path {}
         seen #{}
         explore [[[] (start)]]]
    (if-let [[path state] (first explore)]
      (let [room-data (read-room-data state)
            explore-next (->> room-data
                              :doors
                              (map (fn [direction]
                                     [(conj path direction)
                                      (command state direction)])))
            {:keys [room-name]} room-data]
        (if (and room-name (not (seen room-name)))
          (recur (assoc by-path path room-data)
                 (conj seen room-name)
                 (concat (next explore) explore-next))
          (recur by-path seen (next explore))))
      by-path)))

(def reverse-direction
  {"east" "west"
   "west" "east"
   "north" "south"
   "south" "north"})

(defn go-path [state path]
  (reduce command state path))

(defn path-back [path]
  (->> path
       reverse
       (mapv reverse-direction)))

(defn fetch-item [state item path]
  (-> state
      (go-path path)
      (command (str "take " item))
      (go-path (path-back path))))

(defonce rooms (explore-rooms))

(def forbidden-items
  #{"infinite loop"
    "escape pod"
    "photons"
    "molten lava"
    "giant electromagnet"})

(defn part-1 []
  (let [item-paths (->> rooms
                        (mapcat (fn [[path room]]
                                  (->> room
                                       :items
                                       (map (fn [item]
                                              [item path])))))
                        (remove (comp forbidden-items first))
                        (into {}))
        usable-items (->> item-paths
                          keys
                          (into #{}))
        with-items (reduce (fn [state [item path]]
                             (fetch-item state item path))
                           (start)
                           item-paths)
        path (some (fn [[path room]]
                     (when (= (:room-name room)
                              "Pressure-Sensitive Floor")
                       path))
                   rooms)
        before-sensor (go-path with-items (butlast path))
        sensor-direction (last path)]
    (loop [light-sets #{#{}}
           heavy-sets #{}
           try-sets (into #{}
                          (for [item usable-items]
                            #{item}))]
      (if (empty? try-sets)
        (println "no result found")
        (let [seen (set/union light-sets heavy-sets try-sets)
              keep-items (first try-sets)
              _ (println "Trying" keep-items)
              without-other-items (reduce (fn [state item]
                                            (if (keep-items item)
                                              state
                                              (command state (str "drop " item))))
                                          before-sensor
                                          usable-items)
              after-weighing (command without-other-items sensor-direction)
              {:keys [game-output]} after-weighing
              too-heavy? (boolean
                          (some #(re-find #"Droids on this ship are lighter than the detected value"
                                          %)
                                game-output))
              too-light? (boolean
                          (some #(re-find #"Droids on this ship are heavier than the detected value"
                                          %)
                                game-output))
              next-try-sets (disj try-sets keep-items)]
          (cond
            too-heavy? (recur light-sets
                              (conj heavy-sets keep-items)
                              next-try-sets)
            too-light? (recur (conj light-sets keep-items)
                              heavy-sets
                              (->> light-sets
                                   (map #(set/union % keep-items))
                                   (remove seen)
                                   (into next-try-sets)))))))))

(comment
  (part-1))