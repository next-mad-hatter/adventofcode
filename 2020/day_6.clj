(ns madhat.adventofcode.day-six
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
    p))

(time
 (def nums
   (->>
    "day_5_input.txt"
    slurp
    split-input
    (map parse))))

