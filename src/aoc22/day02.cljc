(ns aoc22.day02
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))


#?(:clj
   (def input (->> "aoc22/day02.txt"
                   io/resource
                   slurp
                   str/split-lines))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day02.txt")
                            (str/split-lines)
                            (map parse-long)))))

(def elf-moves
  {"A" :rock
   "B" :paper
   "C" :scissors})

(def my-moves
  {"X" :rock
   "Y" :paper
   "Z" :scissors})

(def desired-outcome
  {"X" :lose
   "Y" :draw
   "Z" :win})

(def rules
  {:rock :scissors
   :paper :rock
   :scissors :paper})

(def points
  {:rock 1
   :paper 2
   :scissors 3
   :lose 0
   :draw 3
   :win 6})

(def move-for-outcome
  {[:rock :lose] :scissors
   [:rock :draw] :rock
   [:rock :win]  :paper

   [:paper :lose] :rock
   [:paper :draw] :paper
   [:paper :win]  :scissors

   [:scissors :lose] :paper
   [:scissors :draw] :scissors
   [:scissors :win]  :rock})

(defn score-round-1
  [[elf me]]
  (cond
    ;; Elf wins
    (= (get rules elf) me) [(+ (get points elf) (get points :win)) (+ (get points me) (get points :lose))]
    ;; I win
    (= (get rules me) elf) [(+ (get points elf) (get points :lose)) (+ (get points me) (get points :win))]
    ;; Draw
    :else [(+ (get points elf) (get points :draw)) (+ (get points me) (get points :draw))]))

(defn score-round-2
  [round]
  (let [[_ outcome] round
        my-choice (get move-for-outcome round)]
    (+ (get points my-choice) (get points outcome))))

(defn part-1
  "Run with (n)bb -x aoc22.day02/part-1"
  [_]
  (->> input
       (map #(str/split % #" "))
       (map (fn [[elf me]] [(get elf-moves elf)
                            (get my-moves  me)]))
       (map score-round-1)
       (reduce (fn [memo [_ me]] (+ memo me)) 0)
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day02/part-2"
  [_]
  (->> input
       (map #(str/split % #" "))
       (map (fn [[elf me]] [(get elf-moves elf)
                            (get desired-outcome me)]))
       (map score-round-2)
       (apply +)
       prn))
