(ns aoc24.day03
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc24/day03.txt"
                   io/resource
                   slurp))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc24/day03.txt")))))

(defn part-1
  "Run with (n)bb -x aoc24.day03/part-1"
  [_]
  (->> input
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map (fn [[_ a b]] (* (parse-long a) (parse-long b))))
       (apply #'+)
       prn))

(defn part-2
  "Run with (n)bb -x aoc24.day03/part-2"
  [_]
  (->> input
       (re-seq #"do[n\'t]?\(\)|mul\((\d{1,3}),(\d{1,3})\)")
       (reduce
	(fn [{:keys [active total] :as memo} [command val1 val2]]
	  (cond
	    (nil? val1) {:active (= command "do()") :total total}
	    active {:active active :total (+ total (* (parse-long val1) (parse-long val2)))}
	    :else memo))
	{:active true
	 :total 0})
       :total
       (prn)))
