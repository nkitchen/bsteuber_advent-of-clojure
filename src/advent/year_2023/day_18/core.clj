(ns advent.year-2023.day-18.core
  (:require [advent.grid :as grid]
            [advent.tools :as tools]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]))

(defn read-input-part-1 [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (let [[dir steps _] (str/split line #" ")]
                 {:dir (case dir
                         "R" grid/east
                         "L" grid/west
                         "U" grid/north
                         "D" grid/south)
                  :steps (tools/read-int steps)})))))

(defn read-input-part-2 [file]
  (->> file
       tools/read-lines
       (mapv (fn [line]
               (let [hex (-> (str/split line #" ")
                             last
                             (str/replace #"[()#]" ""))
                     dir (case (last hex)
                           \0 grid/east
                           \1 grid/south
                           \2 grid/west
                           \3 grid/north)
                     steps (tools/read-hex
                            (apply str (butlast hex)))]
                 {:dir dir
                  :steps steps})))))

(def horizontal? #{grid/west grid/east})

(defn path-points [path]
  (loop [[line & next-line] path
         current-point [0 0]
         result []
         max-y 0
         max-dir nil]
    (if line
      (let [{:keys [dir steps]} line
            next-point (grid/go-dir current-point
                                    dir
                                    steps)
            [_ y] next-point
            [next-max-y next-max-dir] (if (and (> y max-y)
                                               (horizontal? dir))
                                        [y dir]
                                        [max-y max-dir])]
        (recur
         next-line
         next-point
         (conj result [line current-point])
         next-max-y
         next-max-dir))
      [result max-dir])))

(defn shoelace [path]
  (let [[lines-with-points max-dir] (path-points path)]
    (->> (->> lines-with-points
              cycle
              (partition 3 1)
              (take (count lines-with-points)))
         (map (fn [[before line after]]
                (let [before-dir (:dir (first before))
                      after-dir (:dir (first after))
                      [{:keys [dir steps]} [_ y]] line
                      start-dir? (not= max-dir dir)
                      dec-steps (dec steps)
                      inc-steps (inc steps)
                      steps (when (horizontal? dir)
                              (if start-dir?
                                (condp = [before-dir after-dir]
                                  [grid/north grid/north] steps
                                  [grid/south grid/south] steps
                                  [grid/north grid/south] inc-steps
                                  [grid/south grid/north] dec-steps
                                  (throw (ex-info "Unexpected before / after dir"
                                                  {:before-dir before-dir
                                                   :after-dir after-dir})))
                                (-
                                 (condp = [before-dir after-dir]
                                   [grid/north grid/north] steps
                                   [grid/south grid/south] steps
                                   [grid/north grid/south] dec-steps
                                   [grid/south grid/north] inc-steps
                                   (throw (ex-info "Unexpected before / after dir"
                                                   {:before-dir before-dir
                                                    :after-dir after-dir}))))))]
                  (when steps
                    (* steps (if start-dir?
                               (- y)
                               (- (inc y))))))))
         (filter some?)
         (apply +))))

(pprint (shoelace (read-input-part-1 "test")))

(defn part-1 [file]
  (shoelace (read-input-part-1 file)))

(defn part-2 [file]
  (shoelace (read-input-part-2 file)))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))
