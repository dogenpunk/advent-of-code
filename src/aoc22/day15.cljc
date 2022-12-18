(ns aoc22.day15
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc22/day15.txt"
                   io/resource
                   slurp
                   ))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day15.txt")
                            (str/split-lines)
                            (map parse-long)))))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn parse
  [input]
  (->> input
       (re-seq #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")
       (map (fn [[_ sx sy bx by]]
              [[(parse-long sx) (parse-long sy)]
               [(parse-long bx) (parse-long by)]]))
       (map (fn [[sxy bxy]]
              [sxy (manhattan-distance sxy bxy)]))))

(defn covers-row?
  [ty [[_x y] r]]
  (<= (abs (- y ty)) r))

(defn sensor-row-coverage
  [ty [[x y] r]]
  (let [d (abs (- ty y))
        span (- r d)]
    [(- x span) (+ x span)]))

(defn row-spans-covered
  [sensors ty]
  (sort
   (sequence
    (comp
     (filter #(covers-row? ty %))
     (map #(sensor-row-coverage ty %)))
    sensors)))

(defn combine-spans
  [[a1 a2 :as a] [b1 b2 :as b]]
  ;; assumes [a b] are sorted!
  (cond
    (= a b) [a]
    (= (inc a2) b1) [[a1 b2]]
    (< a2 b1) [a b]
    (<= b1 a2 b2) [[a1 b2]]
    (<= a1 b1 b2 a2) [a]))

(defn combine-all-spans
  [[s1 & sr]]
  (reduce (fn [spans span]
            (into (pop spans) (combine-spans (peek spans) span)))
          [s1]
          sr))

(defn count-covered-spaces
  [spans]
  (transduce
   (map (fn [[y1 y2]] (if (= y1 y2)
                        1
                        (- y2 y1))))
   +
   spans))

(defn find-in-spans
  [spans limit]
  (let [[[a1 a2] [b1 b2]] spans]
    (cond
      (and (nil? b1) (= 1 a1) (<= limit a2))
      0

      (and (nil? b1) (<= a1 0) (= (dec limit) a2))
      limit

      (and (= 2 (count spans)) (<= a1 0) (<= limit b2) (= (+ 2 a2) b1))
      (inc a2))))

(defn find-distress
  [sensors limit]
  (first (for [row (range limit)
               :let [x (find-in-spans (-> sensors
                                          (row-spans-covered row)
                                          combine-all-spans)
                                      limit)]
               :when (some? x)]
           [x row])))

(defn p-find-distress
  [sensors limit threads]
  (let [coords (promise)
        row-atom (atom limit)]
    (dotimes [_t threads]
      (future
        (loop []
          (let [[row _] (swap-vals! row-atom dec)
                spans (-> sensors (row-spans-covered row) combine-all-spans)
                x (find-in-spans spans limit)]
            (if (some? x)
              (deliver coords [x row])
              (when-not (realized? coords) (recur)))))))
    @coords))

(defn signal-freq [x y]
  (+ (* x 4000000) y))


(defn part-1
  "Run with (n)bb -x aoc22.day15/part-1"
  [_]
  (-> input
       parse
       (row-spans-covered 2000000)
       combine-all-spans
       count-covered-spaces
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day15/part-2"
  [_]
  (prn (apply signal-freq (-> input
                              parse
                              (p-find-distress 4000000 12)))))

(comment
  (time (apply signal-freq (-> input
                               parse
                               (find-distress 4000000))))
  (time (apply signal-freq (-> input
                               parse
                               (p-find-distress 4000000 8))))
  )
