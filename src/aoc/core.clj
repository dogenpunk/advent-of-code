(ns aoc.core
  (:require [clojure.string :as str]))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn grid-get [grid [x y]]
  (get-in grid [y x]))

(defn empty-grid
  [w h]
  (vec (repeat h (vec (repeat w \.)))))

(defn mark-point
  [grid [x y] v]
  (assoc-in grid [y x] v))

(defn print-grid [grid]
  (->> (map str/join grid)
       (str/join \newline)
       (println)))

(defn print-points [points]
  (let [max-x (apply max (map first points))
        max-y (apply max (map second points))]
    (-> (reduce #(mark-point %1 %2 \#)
                (empty-grid (inc max-x) (inc max-y))
                points)
        print-grid)))
