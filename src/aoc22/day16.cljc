(ns aoc22.day16
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p]))
  (:import (java.util PriorityQueue)))

#?(:clj
   (def input (->> "aoc22/day16.txt"
                   io/resource
                   slurp
                   ))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day16.txt")
                            (str/split-lines)
                            (map parse-long)))))

(defn parse-graph
  [input]
  (->> (for [line (str/split-lines input)]
         (let [rate (parse-long (re-find #"\d+" line))
               [valve & neighbours] (map keyword (re-seq #"[A-Z]{2}" line))]
           [valve {:rate rate
                   :neighbours (vec neighbours)}]))
       (into {})))

(defn path
  [graph a b]
  (loop [queue [[a [a]]]
         seen #{a}]
    (let [[[c path] & queue] queue]
      (cond
        (not c) []
        (= b c) path
        :else (let [ns (->> graph c :neighbours (remove seen))
                    nx (for [n ns] [n (conj path n)])]
                (recur (concat queue nx)
                       (into seen ns)))))))

(defn distances-between
  [graph valves]
  (->> (for [a valves
             b valves
             :let [p (path graph a b)]
             :when (seq p)]
         {a {b (count p)}})
       (reduce (partial merge-with merge) {})))

(defn index-of-max
  [xs]
  (->> (map vector xs (range))
       (reduce (partial max-key first))
       second))

(defn release-valves
  [graph closed start times]
  (let [distances (distances-between graph (into closed start))
        queue (new PriorityQueue 10000 (comparator (fn [a b] (> (:priority a) (:priority b)))))]
    (.add queue {:positions start
                 :paths (mapv vector start)
                 :times times
                 :closed closed
                 :pressure 0
                 :priority 0})
    (loop [seen #{}]
      (let [{:as state :keys [positions times closed pressure paths]} (.poll queue)]
        (cond
          (seen [(set positions) pressure])
          (recur seen)

          (or (empty? closed) (= 0 (apply max times)))
          state

          :else
          (do
            (.addAll queue
                     (let [index (index-of-max times)
                           valve (positions index)
                           time (times index)]
                       (for [[valve dist] (distances valve)
                             :when (closed valve)
                             :let [positions (assoc positions index valve)
                                   time      (max 0 (- time dist))
                                   times     (assoc times index time)
                                   closed    (disj closed valve)
                                   pressure  (+ pressure (* time (:rate (graph valve))))]
                             :when (not (seen [(set positions) pressure]))]
                         {:positions positions
                          :paths     (update paths index #(conj % valve))
                          :times     times
                          :closed    closed
                          :pressure  pressure
                          :priority  (+ pressure (* 10 (count closed) (apply max times)))})))
            (recur (conj seen [(set positions) pressure]))))))))

(defn solution
  [input start times]
  (let [graph (parse-graph input)
        closed-valves (->> graph (keep (fn [[v {:keys [rate]}]] (when (< 0 rate) v))) set)
        state (release-valves graph closed-valves start times)]
    (:pressure state)))

(time (-> input
          (solution [:AA] [30])
          prn))

(time (-> input
          (solution [:AA :AA] [26 26])
          prn))

(defn part-1
  "Run with (n)bb -x aoc22.day16/part-1"
  [_]
  (->> input
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))
       (apply max)
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day16/part-2"
  [_]
  (->> input
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))
       (sort >)
       (take 3)
       (apply +)
       prn))
