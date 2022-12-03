(ns aoc22.day03
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            [clojure.set :as set]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc22/day03.txt"
                   io/resource
                   slurp
                   str/split-lines
                   ))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day03.txt")
                            (str/split-lines)
                            ))))

(def priorities
  (->> "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
       seq
       (map-indexed (fn [i c] [c (inc i)]))
       (into {})))

(defn get-compartment-contents
  [s]
  (let [l (.length s)]
    [(subs s 0 (/ l 2))
     (subs s (/ l 2))]))

(defn get-common-item
  ([compartments]
   (->> compartments
        (map seq)
        (map #(into #{} %))
        (apply set/intersection)
        first)))

(defn part-1
  "Run with (n)bb -x aoc22.day03/part-1"
  [_]
  (->> input
       (map get-compartment-contents)
       (map get-common-item)
       (reduce (fn [memo item]
                 (+ memo (get priorities item))) 0)
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day03/part-2"
  [_]
  (->> input
       (partition 3)
       (map get-common-item)
       (reduce (fn [memo item]
                 (+ memo (get priorities item))) 0)
       prn))

(comment
  (part-1 nil)
  (part-2 nil))
