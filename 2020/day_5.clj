(ns madhat.adventofcode.day-five
  (:require
   [clojure.string :as str]
   #_[clojure.set :as set]
   #_[clojure.algo.generic.functor :as gf :refer [fmap]]
   #_[clojure.math.combinatorics :as combo]
   #_[clojure.zip :as zip]))

(defn split-input [input]
  (str/split input #"\R"))

(defn parse [p]
  (as-> p $
    (str/replace $ #"F" "0")
    (str/replace $ #"B" "1")
    (str/replace $ #"L" "0")
    (str/replace $ #"R" "1")
    ((juxt #(subs % 0 7) #(subs % 7 10)) $)
    (map #(. Integer parseInt % 2) $)))

(time
 (def nums
   (->>
    "day_5_input.txt"
    slurp
    split-input
    (map parse)
    (into #{}))))

(time
 (def part-1-answer
   (apply max (map (partial apply #(+ (* 8 %1) %2)) nums))))

part-1-answer

nums

(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))

(def part-2-answer)

(apply #(+ (* 8 %1) %2)
       (first
        (for [x (range 256)
              y (range 8)
              :when (not (contains? nums [x y]))
              :when (contains? nums [(dec x) y])
              :when (contains? nums [(inc x) y])]
          [x y])))

part-2-answer
