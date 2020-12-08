(ns madhat.adventofcode
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pp]
   #_[clojure.set :as set]
   #_[clojure.algo.generic.functor :as gf :refer [fmap]]
   #_[clojure.math.combinatorics :as combo]
   #_[ubergraph.core :as uber]
   #_[ubergraph.alg :as ga :refer [topsort]]
   #_[clojure.zip :as zip]))

(defn spy
  ([val] (spy "DBG:" val))
  ([msg val] (print msg " ") (pp/pprint val) val))

(defn parse-input [input]
  (-> input
       str/lower-case
       (str/split #"\R")))

(time
 (def input
   (->> "day_8_test.txt"
        slurp
        parse-input)))

;; Part 1

(time
 (def answer-1
   0))

answer-1

(time
 (def answer-2
   1))

answer-2
