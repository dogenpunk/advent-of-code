(ns aoc22.day08
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc22/day08.txt"
                   io/resource
                   slurp
                   ))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day08.txt")
                            (str/split-lines)
                            (map parse-long)))))

(def test-data
  "30373
25512
65332
33549
35390")

(defn char->n [c]
  (Character/digit c 10))

(defn parse-map
  [m]
  (->> m
       str/split-lines
       (mapv (comp (partial into []) (partial map char->n) seq))))

(defn look-row [row]
  (let [l (count row)]
    (->> row
         (map-indexed (fn [i t]
                        (if (or (= 0 i)
                                (= (dec l) i)) ;; edges are always visible
                          :visible
                          (if (or (> t (apply max (subvec row 0 i)))
                                  (> t (apply max (subvec row (inc i) l))))
                            :visible
                            :hidden))))
         (apply vector))))

(defn transpose [m]
  (apply mapv vector m))

(defn find-visible
  [m]
  (let [horizontal (mapv look-row m)
        vertical   (transpose (mapv look-row (transpose m)))]
    (for [x (range 0 (count (nth m 0)))
          y (range 0 (count m))]
      (if (and (= (nth (nth horizontal y) x) :hidden)
               (= (nth (nth vertical   y) x) :hidden))
        :hidden
        :visible))))

(defn directions
  [rows columns [y x]]
  [(reverse (subvec (nth columns x) 0 y)) ;; top
   (subvec (nth rows y) (inc x) (count (first rows)))   ;; right
   (subvec (nth columns x) (inc y) (count (first columns))) ;; bottom
   (reverse (subvec (nth rows y) 0 x)) ;; left
   ])

(defn count-visible-trees
  [origin trees]
  (let [visible (take-while #(> origin %) trees)]
    (if (= visible trees) (count visible) (inc (count visible)))))

(defn score
  [rows columns points]
  (let [tree (get-in rows points)]
    (->> points
         (directions rows columns)
         (map (partial count-visible-trees tree)))))

(defn calculate-scenic-score
  [rows]
  (let [points (for [x (range 0 (count (nth rows 0)))
                     y (range 0 (count rows))]
                 [x y])
        cols (transpose rows)]
    (map (partial score rows cols) points)))

(defn part-1
  "Run with (n)bb -x aoc22.day08/part-1"
  [_]
  (->> input
       parse-map
       find-visible
       (filter #(= :visible %))
       count
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day08/part-2"
  [_]
  (->> input
       parse-map
       calculate-scenic-score
       (map (partial apply *))
       (apply max)
       prn))
