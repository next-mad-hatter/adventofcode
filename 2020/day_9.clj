(ns madhat.adventofcode
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))

;; (def input-file "day_9_test.txt")
;; (def cont-length 5)
(def input-file "day_9_input.txt")
(def cont-length 25)

(defn find-first [f coll]
  (first (filter f coll)))

(defn parse-input [input]
  (as-> input $
    (str/lower-case $)
    (str/split $ #"\R")
    (map #(Long/parseLong %) $)))

(time
 (def input
   (->> input-file
        slurp
        parse-input)))

;; Part 1

(defn sums-of-pairs [xs]
  (let [s (set xs)
        c (combo/combinations s 2)]
    (into #{} (map (partial apply +) c))))

(defn find-part-1 [n]
  (->> input
       (partition (+ n 1) 1 nil)
       (map reverse)
       (map (juxt first (comp sums-of-pairs rest)))
       (filter (fn [[k v]] (not (v k))))))

(time
 (def answer-1 (ffirst (find-part-1 cont-length))))

answer-1
;; => 127
;; => 25918798

;;  Part 2

(defn starts-with [n s coll]
  (loop [n n
         s s
         prefix []]
    (let [x (nth coll n)]
      (cond
        (and (= x s) (empty? prefix)) nil
        (= x s) prefix
        (> x s) nil
        (>= (inc n) (count coll)) nil
        :else (recur (inc n) (- s x) (conj prefix x))))))

(time
 (def answer-2
   (->> input
        count
        range
        (map #(starts-with % answer-1 input))
        (find-first some?)
        ((juxt (partial apply min) (partial apply max)))
        (apply +))))

answer-2
;; => 62
;; => 3340942
