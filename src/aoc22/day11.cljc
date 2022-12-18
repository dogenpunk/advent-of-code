(ns aoc22.day11
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc22/day11.txt"
                   io/resource
                   slurp))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day11.txt")
                            (str/split-lines)
                            (map parse-long)))))

(defn parse
  [input]
  (->> input
       (re-seq #"(Monkey) (\d+):|(Starting items:) ([0-9, ]+)|(Operation): new = old (\W) (\d+|old)|Test: (\w+) by (\d+)|(true|false): throw to monkey (\d+)")
       (map (partial drop 1))
       (map (partial remove nil?))
       (partition 5)
       ))

(defn solve-1
  [lines]
  (let [parse-operation (fn [s]
                          (let [[_ _ x op y] (re-seq #"\w+|\+|\*" s)
                                op (resolve (symbol op))]
                            (if (= x y)
                              (fn [x] (op x x))
                              (fn [x] (op x (parse-long y))))))
        parse-monkey (fn [line]
                       (let [[_ items op test mt mf] (str/split-lines line)
                             [div m1 m2] (->> (str test mt mf)
                                              (re-seq #"-?\d+")
                                              (mapv parse-long))]
                         {:items (->> items (re-seq #"-?\d+") (mapv parse-long))
                          :div div
                          :op (parse-operation op)
                          :test #(if (zero? (mod % div)) m1 m2)}))
        round (fn [reduce-worry init-state]
                (let [turn (fn [state current]
                             (let [{:keys [items op test]} (nth state current)]
                               (-> (reduce (fn [state item]
                                             (let [item' (reduce-worry (op item))
                                                   to    (test item')]
                                               (update-in state [to :items] conj item')))
                                           state
                                           items)
                                   (assoc-in [current :items] [])
                                   (update-in [current :inspected] (fnil + 0) (count items)))))]
                  (reduce turn init-state (range (count init-state)))))
        monkeys (mapv parse-monkey lines)]
    (->> monkeys
         (iterate (partial round #(quot % 3)))
         (drop 20)
         (first)
         (map :inspected)
         (sort >)
         (take 2)
         (apply *))))

(defn solve-2
  [lines]
  (let [parse-operation (fn [s]
                          (let [[_ _ x op y] (re-seq #"\w+|\+|\*" s)
                                op (resolve (symbol op))]
                            (if (= x y)
                              (fn [x] (op x x))
                              (fn [x] (op x (parse-long y))))))
        parse-monkey (fn [line]
                       (let [[_ items op test mt mf] (str/split-lines line)
                             [div m1 m2] (->> (str test mt mf)
                                              (re-seq #"-?\d+")
                                              (mapv parse-long))]
                         {:items (->> items (re-seq #"-?\d+") (mapv parse-long))
                          :div div
                          :op (parse-operation op)
                          :test #(if (zero? (mod % div)) m1 m2)}))
        round (fn [reduce-worry init-state]
                (let [turn (fn [state current]
                             (let [{:keys [items op test]} (nth state current)]
                               (-> (reduce (fn [state item]
                                             (let [item' (reduce-worry (op item))
                                                   to    (test item')]
                                               (update-in state [to :items] conj item')))
                                           state
                                           items)
                                   (assoc-in [current :items] [])
                                   (update-in [current :inspected] (fnil + 0) (count items)))))]
                  (reduce turn init-state (range (count init-state)))))
        monkeys (mapv parse-monkey lines)
        lcm (->> monkeys (map :div) (reduce *))]
    (->> monkeys
         (iterate (partial round #(mod % lcm)))
         (drop 10000)
         (first)
         (map :inspected)
         (sort >)
         (take 2)
         (apply *))))

(defn part-1
  "Run with (n)bb -x aoc22.day11/part-1"
  [_]
  (->> (str/split input #"\R\R")
       (solve-1)
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day11/part-2"
  [_]
  (->> (str/split input #"\R\R")
       (solve-2)
       prn))
