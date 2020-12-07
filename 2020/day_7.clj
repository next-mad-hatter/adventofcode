(ns madhat.adventofcode.day-six
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.algo.generic.functor :as gf :refer [fmap]]
   [ubergraph.core :as uber]
   [clojure.pprint :as pp]
   #_[ubergraph.alg :as ga :refer [topsort]]
   #_[clojure.math.combinatorics :as combo]
   #_[clojure.zip :as zip]))

;; TODO: check out cider debugging & debux, specter & instaparse
;; TODO: cider-interrupt does not seem to work with my current doom-emacs setup

(defn spy
  ([val] (spy "DBG:" val))
  ([msg val] (print msg " ") (pp/pprint val) val))

(defn parse-child [description]
  (->> description
       ;; TODO: can we make match this non-eagerly and not rely on str/split below?
       (re-matches #"(\d+)\s+(.+)\s+bags?")
       next
       (apply #(vector %2 (Long/parseLong %1)))))

(defn parse-input [input]
  (->> input
       str/lower-case
       (re-seq #"(.+)\s+bags contain\s+(.+)\R")
       (map rest)
       (map vec)
       (into {})
       (gf/fmap #(str/split % #"\s*[.,]\s*"))
       (gf/fmap #(remove #{"no other bags"} %))
       (gf/fmap (partial map parse-child))
       (gf/fmap #(into {} %))))

(time
 (def graph
   (->> "day_7_input.txt"
        slurp
        parse-input
        uber/digraph)))

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

;; TODO: memoize + detect loops
(defn count-contents [g]
  (fn [n]
    (let [ms       (uber/successors g n)
          ws       (map (partial uber/weight graph n) ms)
          subsums  (map (count-contents g) ms)
          subtotal (apply + (map * ws subsums))]
      (+ (apply + ws) subtotal))))

(def counter (count-contents graph))

(time
 (def answer-2
   (counter "shiny gold")))

answer-2 ;; => 89084
