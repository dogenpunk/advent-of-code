(ns aoc.day-one
  (:require [clojure.java.io :as io]))

(->> (io/resource "day-one-data.txt")
     io/reader
     line-seq
     (map #(Integer/parseInt %))
     (partition 2 1)
     (filter (fn [[a b]] (< a b)))
     count)

(->> (io/resource "day-one-data.txt")
     io/reader
     line-seq
     (map #(Integer/parseInt %))
     (partition 3 1)
     (map (partial apply +))
     (partition 2 1)
     (filter (fn [[a b]] (< a b)))
     count)
