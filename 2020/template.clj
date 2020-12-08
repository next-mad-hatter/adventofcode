(ns madhat.adventofcode.day-six
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.algo.generic.functor :as gf :refer [fmap]]
   #_[clojure.math.combinatorics :as combo]
   #_[ubergraph.core :as uber]
   #_[ubergraph.alg :as ga :refer [topsort]]
   #_[clojure.zip :as zip]))

(defn spy
  ([val] (spy "DBG:" val))
  ([msg val] (print msg " ") (pp/pprint val) val))

(defn parse-input [input]
  (->> input
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
   (count (all-predecessors graph "shiny gold"))))

answer-1 ;; => 185

;; Part 2

(defn count-contents [g]
  (memoize
   (fn [n]
     (let [ms       (uber/successors g n)
           ws       (map (partial uber/weight graph n) ms)
           subsums  (map (count-contents g) ms)
           subtotal (apply + (map * ws subsums))]
       (+ (apply + ws) subtotal)))))

(def counter (count-contents graph))

(time
 (def answer-2
   (counter "shiny gold")))

answer-2 ;; => 89084
