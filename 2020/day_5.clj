(ns madhat.adventofcode.day-three
  (:require
   [clojure.string :as str]
   #_[clojure.set :as set]
   #_[clojure.algo.generic.functor :as gf :refer [fmap]]
   #_[clojure.math.combinatorics :as combo]))

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
    (map parse))))

(time
 (def part-1-answer
   (apply max (map (partial apply #(+ (* 8 %1) %2)) nums))))

part-1-answer
