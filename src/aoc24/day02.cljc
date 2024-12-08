(ns aoc24.day02
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc24/day02.txt"
                   io/resource
                   slurp
                   str/split-lines
                   (map (fn [l]
			  (let [nums (str/split l #"\s+")]
			    (map parse-long nums))))))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc24/day02.txt")
                            (str/split-lines)
                            (map (fn [l]
			  (let [nums (str/split l #"\s+")]
			    (map parse-long nums))))))))

(defn levels-all-increasing?
  [lvls]
  (apply #'< lvls))

(defn levels-all-decreasing?
  [lvls]
  (apply #'> lvls))

(defn levels-all-differ-by-1-to-3?
  [lvls]
  (let [diffs (map (fn [[a b]]
		     (abs (- a b)))
		   (partition 2 1 lvls))]
    (<= 1 (apply #'min diffs) (apply #'max diffs) 3)))

(defn report-safe?
  [report]
  (and (or (levels-all-increasing? report)
	   (levels-all-decreasing? report))
       (levels-all-differ-by-1-to-3? report)))

(defn remove-nth
  [n coll]
  (keep-indexed #(when (not= %1 n) %2) coll))

(defn problem-dampener
      [report]
  (if (report-safe? report)
    true
    (->> (count report)
	 (range)
	 (map (fn [n] (remove-nth n report)))
	 (some report-safe?))))

(defn part-1
      "Run with (n)bb -x aoc24.day02/part-1"
      [_]
      (->> input
	   (filter report-safe?)
	   (count)
	   (prn)))



(defn part-2
      "Run with (n)bb -x aoc24.day02/part-2"
      [_]
  (->> input
       (filter problem-dampener)
       (count)
       (prn)))
