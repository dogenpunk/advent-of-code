(ns aoc22.day12
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p]))
  (:import (clojure.lang PersistentQueue IPersistentStack IPersistentCollection)))

#?(:clj
   (def input (->> "aoc22/day12.txt"
                   io/resource
                   slurp
                   ))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day12.txt")
                            (str/split-lines)
                            (map parse-long)))))

(def test-input
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn parse [s]
  (mapv vec (str/split-lines s)))

(defn grid-val [grid [x y]] (get (get grid x) y))

(defn height [grid pos]
  (first
   (replace {83 97, 69 122} [(int (or (grid-val grid pos) 123))])))

(defn possible-moves [grid pos]
  (->> [[1 0] [-1 0] [0 1] [0 -1]]
       (filter #(>= 1 (- (height grid (mapv + pos %)) (height grid pos))))
       (map #(mapv + pos %))))

(defn find-in [grid char]
  (->> (for [x (range (count grid))
             y (range (count (grid x)))]
         [x y])
       (filter #(= char (grid-val grid %)))
       set))

(defn get-distance [grid start-char goal-char]
  (let [starts (find-in grid start-char)
        goals (find-in grid goal-char)]
    (loop [choices starts
           checked starts
           counter 0]
      (if (some #(contains? goals %) choices)
        counter
        (recur (remove checked
                       (set (mapcat #(possible-moves grid %) choices)))
               (into checked choices)
               (inc counter))))))

(defn part-1
  "Run with (n)bb -x aoc22.day12/part-1"
  [_]
  (prn (get-distance (parse input) \S \E)))

(defn part-2
  "Run with (n)bb -x aoc22.day12/part-2"
  [_]
  (prn (get-distance (parse input) \a \E)))
