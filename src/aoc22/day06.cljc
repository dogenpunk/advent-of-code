(ns aoc22.day06
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc22/day06.txt"
                   io/resource
                   slurp
                   ))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day06.txt")
                            (str/split-lines)
                            (map parse-long)))))

(defn find-unique-seq-indexes
  [len s]
  (for [index (range 0 (inc (count s)))
        :let [chunk (take len (drop index s))]
        :when (= len (count (into #{} chunk)))]
    (+ index len)))

(defn part-1
  "Run with (n)bb -x aoc22.day06/part-1"
  [_]
  (->> input
       (find-unique-seq-indexes 4)
       first
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day06/part-2"
  [_]
  (->> input
       (find-unique-seq-indexes 14)
       first
       prn))
