(ns aoc22.day02
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))


#?(:clj
   (def input (->> "aoc22/day02.txt"
                   io/resource
                   slurp
                   str/split-lines
                   (map #(str/split % #" "))))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day02.txt")
                            (str/split-lines)
                            (map parse-long)))))

(def elf-moves
  {"A" :rock
   "B" :paper
   "C" :scissors})

(defn get-elf-move
  [[move _]]
  (get elf-moves move))

(def my-moves
  {"X" :rock
   "Y" :paper
   "Z" :scissors})

(defn get-my-move
  [[_ move]]
  (get my-moves move))

(def desired-outcome
  {"X" :lose
   "Y" :draw
   "Z" :win})

(defn get-desired-outcome
  [[_ outcome]]
  (get desired-outcome outcome))

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
    (= (get rules elf) me) (+ (get points me) (get points :lose))
    ;; I win
    (= (get rules me) elf) (+ (get points me) (get points :win))
    ;; Draw
    :else (+ (get points me) (get points :draw))))

(defn score-round-2
  [round]
  (let [[_ outcome] round
        my-choice (get move-for-outcome round)]
    (+ (get points my-choice) (get points outcome))))

(defn part-1
  "Run with (n)bb -x aoc22.day02/part-1"
  [_]
  (->> input
       (map (juxt get-elf-move get-my-move))
       (map score-round-1)
       (apply +)
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day02/part-2"
  [_]
  (->> input
       (map (juxt get-elf-move get-desired-outcome))
       (map score-round-2)
       (apply +)
       prn))

(comment
  (part-1 nil)
  (part-2 nil))
