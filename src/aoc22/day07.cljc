(ns aoc22.day07
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.zip :as zip]
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> "aoc22/day07.txt"
                   io/resource
                   slurp
                   str/split-lines))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day07.txt")
                            (str/split-lines)
                            (map parse-long)))))

(defn mkdir
  [name]
  {:type :dir :name name :children '()})

(defn touch
  [name size]
  {:type :file :name name :size size})

(defn dir?
  [loc]
  (= :dir (:type loc)))

(defn target-dir?
  [loc target]
  (and (dir? (zip/node loc))
       (= target (:name (zip/node loc)))))

(defn cd
  [loc target]
  (condp = target
    ".." (zip/up loc)
    "/" (->> (iterate zip/up loc)
             (take-while #(not (nil? %)))
             last)
    (->> (zip/down loc)
         (iterate zip/next)
         (drop-while #(and (not (zip/end? %))
                           (not (target-dir? % target))))
         first)))

(defn parse-cmd
  [s]
  (-> s
      (str/replace #"^\$ " "")
      (str/split #" ")))

(defn exec-cmd
  [fs raw]
  (let [[a b] (parse-cmd raw)]
    (case a
      "ls" fs
      "cd" (cd fs b)
      "dir" (zip/append-child fs (mkdir b))
      (zip/append-child fs (touch b (parse-long a))))))

(defn append-files
  [dir files]
  (assoc dir :children files))

(defn make-zipper
  [root]
  (zip/zipper dir? :children append-files root))

(def fs (let [tree (make-zipper (mkdir "/"))]
          (->> (rest input)
               (reduce exec-cmd tree)
               zip/root)))

(defn du
  [tree]
  (reduce (fn [sum [k v]]
            (case k
              :size (+ sum v)
              :children (apply + sum (map du v))
              sum)) 0 tree))

(defn part-1
  "Run with (n)bb -x aoc22.day07/part-1"
  [_]
  (->> fs
       (tree-seq dir? :children)
       (filter dir?)
       (map du)
       (filter #(<= % 100000))
       (apply +)
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day07/part-2"
  [_]
  (let [sizes  (->> fs
                    (tree-seq dir? :children)
                    (filter dir?)
                    (map du))
        root   (du fs)
        total  70000000
        target 30000000
        needed (- target (- total root))]
    (->> sizes
         (filter #(<= needed %))
         sort
         first
         prn)))
