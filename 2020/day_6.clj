(ns madhat.adventofcode.day-six
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   #_[clojure.algo.generic.functor :as gf :refer [fmap]]
   #_[clojure.math.combinatorics :as combo]
   #_[clojure.zip :as zip]))

(defn split-input [input]
  (str/split input #"\R\s*\R"))

(defn parse-1 [p]
  (as-> p $
    (into #{} $)
    (remove #{\newline} $)))

(defn parse-2 [p]
  (as-> p $
    (str/split $ #"\R")
    (map (partial into #{}) $)
    (apply set/intersection $)))

(defn solver [parser]
  (fn [input]
     (->> input
      slurp
      split-input
      (map parser)
      (map count)
      (apply +))))

(def part-1-answer
  (time
   ((solver parse-1) "day_6_input.txt")))

(def part-2-answer
  (time
   ((solver parse-2) "day_6_input.txt")))

part-1-answer
;; => 6778

part-2-answer
;; => 3406
