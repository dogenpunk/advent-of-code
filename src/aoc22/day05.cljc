(ns aoc22.day05
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc22/day05.txt"
                   io/resource
                   slurp
                   str/split-lines
                   ))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day05.txt")
                            (str/split-lines)
                            (map parse-long)))))

(defn parse-row
  [row]
  (re-seq #"[\[\]A-Z ]{3}\s?" row))
(->> input (take 8) (map parse-row))

(defn transpose
  [m]
  (apply mapv vector m))

(defn parse-move
  [move]
  (let [amt (parse-long (second (re-find #"move\s(\d+)" move)))
        from (parse-long (second (re-find #"from\s(\d)" move)))
        to (parse-long (second (re-find #"to\s(\d)" move)))]
    {:amt amt
     :from (dec from)
     :to (dec to)}))

(defn process-move-9000
  [crates {:keys [amt from to]}]
  (-> crates
      (update to concat (reverse (take-last amt (get crates from))))
      (update from #(drop-last amt %))))

(defn process-move-9001
  [crates {:keys [amt from to]}]
  (-> crates
      (update to concat  (take-last amt (get crates from)))
      (update from #(drop-last amt %))))

(defn rebalance-cargo-9000
  [input]
  (let [crates  (->> input (take 8) (map parse-row) transpose (map reverse) (map #(remove str/blank? %)) (into (vector)))
        moves (->> input (drop 9) rest (map parse-move))]
    (reduce process-move-9000 crates moves)))

(defn rebalance-cargo-9001
  [input]
  (let [crates  (->> input (take 8) (map parse-row) transpose (map reverse) (map #(remove str/blank? %)) (into (vector)))
        moves (->> input (drop 9) rest (map parse-move))]
    (reduce process-move-9001 crates moves)))

(defn part-1
  "Run with (n)bb -x aoc22.day05/part-1"
  [_]
  (->> input
       rebalance-cargo-9000
       (map last)
       (map #(re-find #"[A-Z]{1}" %))
       str/join
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day05/part-2"
  [_]
  (->> input
       rebalance-cargo-9001
       (map last)
       (map #(re-find #"[A-Z]{1}" %))
       str/join
       prn))

(comment
  (part-1 nil)
  (part-2 nil))
