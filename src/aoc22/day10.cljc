(ns aoc22.day10
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc22/day10.txt"
                   io/resource
                   slurp
                   (re-seq #"(-?\d+)|(noop)")))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day10.txt")
                            (str/split-lines)
                            (map parse-long)))))

(def test-input
  (->> "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"
       (re-seq #"(-?\d+)|(noop)")))

(def signals
  (->> input
       (reduce (fn [register [_ val _]]
                 (let [v (peek register)]
                   (if val
                     (conj register v (+ v (parse-long val)))
                     (conj register v))))
               [1])
       (into [])))

(defn part-1
  "Run with (n)bb -x aoc22.day10/part-1"
  [_]
  (->> [20 60 100 140 180 220]
       (reduce
        (fn [total idx]
          (+ total (* idx (nth signals (dec idx)))))
        0)
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day10/part-2"
  [_]
  (doseq [line (->> signals
                    (map-indexed
                     (fn [i v] (if (#{-1 0 1} (- v (mod i 40))) "#" ".")))
                    (partition 40))]
    (println (str/join line))))
