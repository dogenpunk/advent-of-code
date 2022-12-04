(ns aoc22.day04
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc22/day04.txt"
                   io/resource
                   slurp
                   str/split-lines
                   (map #(str/split % #","))))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day04.txt")
                            (str/split-lines)
                            (map parse-long)))))

(defn rangify
  [[a b]]
  (let [[a1 a2] (str/split a #"-")
        [b1 b2] (str/split b #"-")]
    [(range (parse-long a1) (inc (parse-long a2)))
     (range (parse-long b1) (inc (parse-long b2)))]))

(defn fully-contains?
  [[a b]]
  (or (and (>= (first a)
               (first b))
           (<= (last a)
               (last b)))
      (and (>= (first b)
               (first a))
           (<= (last b)
               (last a)))))

(defn overlaps?
  [[a b]]
  (and (<= (first a)
           (last b))
       (>= (last a)
           (first b))))

(defn part-1
  "Run with (n)bb -x aoc22.day04/part-1"
  [_]
  (->> input
       (map rangify)
       (filter fully-contains?)
       (count)
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day04/part-2"
  [_]
  (->> input
       (map rangify)
       (filter overlaps?)
       (count)
       prn))

(comment
  (part-1 nil)
  (part-2 nil)
  )
