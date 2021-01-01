(ns aoc-2020.day-07
  (:require [common.util :as util]
            [clojure.set :as set]
            [clojure.algo.generic.functor :as gf :refer [fmap]]
            [ubergraph.core :as uber]
            [clojure.pprint :as pp]
            [ubergraph.alg :refer [topsort]]))

;; TODO: check out loom, which provides subgraph-reachable-from & nested-count
;;       functions which we could use instead of rolling our own below

(defn spy
  ([val] (spy "DBG:" val))
  ([msg val] (print msg " ") (pp/pprint val) val))

(defn parse-children [description]
  (->> description
       (re-seq #"(\d+)\s+(.+?)\s+bags?")
       (map #(->> % rest vec))
       (map (partial apply #(vector %2 (Long/parseLong %1))))
       (into {})))

(defn parse-input [input]
  (->> input
       util/fetch-whole
       (re-seq #"(.+)\s+bags contain\s+(.+)\R")
       (map rest)
       (map vec)
       (into {})
       (gf/fmap parse-children)))

(time
 (def graph
   (->> "2020/day_07_input.txt"
        parse-input
        uber/digraph)))

(if-not (topsort graph)
  (throw (Exception. "Could not sort input -- maybe it contains a loop?")))

;; Part 1

(defn all-predecessors [g n]
  (remove #{n}
          (loop [pre #{n}]
            (let [add (set (mapcat (partial uber/predecessors g) pre))
                  new (set/union pre add)]
              (if (= new pre) pre (recur (set/union new pre)))))))

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
