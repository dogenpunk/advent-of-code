(ns aoc22.day09
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.core :as grid]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc22/day09.txt"
                   io/resource
                   slurp
                   (re-seq #"\w+")
                   (map edn/read-string)
                   (partition 2)))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day09.txt")
                            (re-seq #"\w+")
                            (map edn/read-string)
                            (partition 2)))))

(def test-input "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")

(def deltas {'U [0 1] 'D [0 -1] 'L [-1 0] 'R [0 -1]})

(defn expand
  [directions]
  (mapcat
   (fn [[direction n]]
     (take n (repeat direction)))
   directions))

(defn move-point
  [direction [x y]]
  ({'U [x (inc y)]
    'D [x (dec y)]
    'L [(dec x) y]
    'R [(inc x) y]} direction))

(defn distance-between
  [[x1 y1][x2 y2]]
  (let [dx (abs (- x2 x1))
        dy (abs (- y2 y1))]
    (max dx dy)))

(defn move
  [[head tail] direction]
  (let [new-head (move-point direction head)
        new-tail (if (> (distance-between tail new-head) 1)
                   head
                   tail)]
    [new-head new-tail]))

(defn avg
  [a b]
  (/ (+ a b) 2))

(defn move-knot
  [[x1 y1][x2 y2]]
  (let [dx (abs (- x1 x2))
        dy (abs (- y1 y2))]
    [(if (> dx 1) (avg x1 x2) x2)
     (if (> dy 1) (avg y1 y2) y2)]))

(defn move
  [[head & tail] direction]
  (let [new-head (move-point direction head)]
    (loop [prev new-head
           tail tail
           new [new-head]]
      (if-let [knot (first tail)]
        (let [new-knot (if (> (distance-between prev knot) 1)
                         (move-knot knot prev)
                         knot)]
          (if (= new-knot knot)
            (into new tail)
            (recur new-knot (rest tail) (conj new new-knot))))
        new))))

(defn create-rope
  [n]
  (vec (repeat n [0 0])))


(defn part-1
  "Run with (n)bb -x aoc22.day09/part-1"
  [_]
  (->> input
     (expand)
     (reductions move (create-rope 2))
     (map peek)
     set
     count
     prn))

(defn part-2
  "Run with (n)bb -x aoc22.day09/part-2"
  [_]
  (->> input
     (expand)
     (reductions move (create-rope 10))
     (map peek)
     set
     count
     prn))
