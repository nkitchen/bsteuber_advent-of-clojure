(ns advent.year-2023.day-19.core
  (:require [advent.tools :as tools]
            [clojure.string :as str]))

(defn read-input [file]
  (let [[workflow-lines part-lines] (tools/read-blocks file)]
    {:workflows
     (->> workflow-lines
          (map
           (fn [line]
             (let [[workflow-name rules] (-> line
                                             (str/split #"[{}]"))]
               [workflow-name
                (->> (str/split rules #",")
                     (mapv (fn [rule-str]
                             (let [[lhs rhs] (str/split rule-str #":")]
                               (if rhs
                                 (let [op (cond
                                            (re-find #"<" lhs) "<"
                                            (re-find #">" lhs) ">")
                                       [category number] (str/split lhs #"[<>]")
                                       number (tools/read-int number)]
                                   [[category op number] rhs])
                                 [nil lhs])))))])))
          (into {}))
     :parts (->> part-lines
                 (mapv (fn [line]
                         (->> (-> line
                                  (str/replace #"[{}]" "")
                                  (str/split #","))
                              (map (fn [category-str]
                                     (let [[category number] (str/split category-str #"=")]
                                       [category (tools/read-int number)])))
                              (into {})))))}))

(defn next-workflow [workflows part current]
  (when-not (#{"A" "R"} current)
    (->> (workflows current)
         (some (fn [[constraint next-workflow]]
                 (if-let [[category op number] constraint]
                   (let [op-fn (case op
                                 "<" <
                                 ">" >)]
                     (when (op-fn (get part category) number)
                       next-workflow))
                   next-workflow))))))

(defn part-1 [file]
  (let [{:keys [workflows parts]} (read-input file)]
    (->> parts
         (filter (fn [part]
                   (->> (iterate (partial next-workflow workflows part)
                                 "in")
                        (take-while some?)
                        (take 50)
                        last
                        (= "A"))))
         (mapcat vals)
         (apply +))))

(defn next-states-part-2 [workflows [workflow categories]]
  (loop [[rule & more-rules] (get workflows workflow)
         categories categories
         results []]
    (let [[constraint next-workflow] rule]
      (if-let [[category op number] constraint]
        (let [op-fn (case op "<" < ">" >)
              by-res (group-by #(op-fn % number)
                               (get categories category))
              passes (into #{} (by-res true))
              fails (into #{} (by-res false))
              next-res (if (empty? passes)
                         results
                         (conj results
                               [next-workflow (assoc categories category passes)]))]
          (if (empty? fails)
            next-res
            (recur more-rules
                   (assoc categories category fails)
                   next-res)))
        (conj results [next-workflow categories])))))

(defn count-solutions [sets]
  (->> sets
       (map (fn [set]
              (->> set
                   vals
                   (map count)
                   (apply *))))
       (apply +)))

(defn part-2 [file]
  (let [{:keys [workflows]} (read-input file)]
    (loop [[state & more-states] [["in" (zipmap ["x" "m" "a" "s"]
                                                (repeat (into #{} (range 1 4001))))]]
           accept []]
      (if-let [[workflow categories] state]
        (case workflow
          "A" (recur more-states
                     (conj accept categories))
          "R" (recur more-states
                     accept)
          (recur (concat more-states (next-states-part-2 workflows state))
                 accept))
        (count-solutions accept)))))

(comment
  (part-1 "test")
  (part-1 "input")
  (part-2 "test")
  (part-2 "input"))