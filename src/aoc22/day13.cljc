(ns aoc22.day13
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.edn :as edn]
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc22/day13.txt"
                   io/resource
                   slurp
                   ))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day13.txt")
                            (str/split-lines)
                            (map parse-long)))))

(def test-packets
  (->> (str/split "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]" #"\R\R")
       (map #(edn/read-string (str "[" % "]")))))

(defn parse-packets
  [input]
  (edn/read-string (str "[" input "]")))

(defn cmp-packets
  [left right]
  (cond
     (and (number? left) (number? right)) (compare left right)

     (number? left) (cmp-packets [left] right)

     (number? right) (cmp-packets left [right])

     :else
     (if-let [order (->> (map cmp-packets left right)
                         (filter (comp not zero?))
                         (first))]
       order
       (compare (count left) (count right)))))

(defn part-1
  "Run with (n)bb -x aoc22.day13/part-1"
  [_]
  (->> input
       parse-packets
       (partition 2)
       (keep-indexed (fn [i [left right]]
                       (when (= -1 (cmp-packets left right))
                         (inc i))))
       (reduce + 0)
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day13/part-2"
  [_]
  (let [dividers #{[[2]] [[6]]}]
    (->> input
         parse-packets
         (concat dividers)
         (sort cmp-packets)
         (keep-indexed (fn [i p]
                         (when (dividers p)
                           (inc i))))
         (reduce * 1)
         prn)))
