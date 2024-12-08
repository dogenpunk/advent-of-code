(ns aoc24.day01
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc24/day01.txt"
                   io/resource
                   slurp
                   str/split-lines
                   (map (fn [l]
			  (let [numbers (str/split l #"\s+")]
			    [(parse-long (first numbers))
			     (parse-long (second numbers))])))))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc24/day01.txt")
                            (str/split-lines)
                            (map (fn [l]
				   (let [numbers (str/split l #"\s+")]
				     [(parse-long (first numbers))
				      (parse-long (second numbers))])))))))

(defn part-1
  "Run with (n)bb -x aoc24.day01/part-1"
  [_]
  (let [left (sort (map first input))
	right (sort (map last input))
	solution (apply #'+ (map (fn [l r] (abs (- l r))) left right))]
    (prn solution)))

(defn part-2
  "Run with (n)bb -x aoc24.day01/part-2"
  [_]
  (let [left (map first input)
	right (frequencies (map last input))
	solution (apply #'+ (map (fn [l]
		      (let [r (get right l 0)]
			(* l r))) left))]
    (prn solution)))

