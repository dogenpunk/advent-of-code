(ns aoc.day-two
  (:require [clojure.java.io :as io]))

(->> (io/resource "day-two-data.txt")
     io/reader
     line-seq
     (into [])
     (map #(clojure.string/split % #"\s"))
     (reduce (fn [memo [dir dist]]
               (let [amt (Integer/parseInt dist)]
                 (condp = dir
                   "forward" (update-in memo [0] + amt)
                   "down"    (update-in memo [1] + amt)
                   "up"      (update-in memo [1] - amt)
                   memo)))
             [0 0])
     (apply *))

(def course
  [["forward" "5"]
   ["down" "5"]
   ["forward" "8"]
   ["up" "3"]
   ["down" "8"]
   ["forward" "2"]])


;; [horizontal-pos depth aim]
(->> (io/resource "day-two-data.txt")
     io/reader
     line-seq
     (into [])
     (map #(clojure.string/split % #"\s"))
     (reduce (fn [memo [dir dist]]
               (let [amt (Integer/parseInt dist)]
                 (condp = dir
                   "forward" (-> memo
                                 (update-in [0] + amt)
                                 (update-in [1] + (* (last memo) amt)))
                   "down"    (update-in memo [2] + amt)
                   "up"      (update-in memo [2] - amt)
                   memo)))
             [0 0 0])
     (take 2)
     (apply *))
