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
    (mapv #(Long/parseLong %) $)))

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

(defn pos-yields-sum [nums pos sum]
  (loop [pos pos
         sum sum
         l-min nil
         l-max nil]
    (let [x (get nums pos)]
      (cond
        (nil? x) nil
        (> x sum) nil
        (and (= x sum) (nil? l-min)) nil
        (= x sum) [l-min l-max]
        :else (recur
               (inc pos)
               (- sum x)
               (apply min (remove nil? [l-min x]))
               (apply max (remove nil? [l-max x])))))))

(defn scan-chunk [nums target]
   (->> nums
        count
        range
        (map #(pos-yields-sum nums % target))
        (find-first some?)))

(time
 (def answer-2
   (let [acceptable (fn [x] (< x answer-1))]
     (->> input
          (partition-by acceptable)
          (filter (comp acceptable first))
          (filter #(< 1 (count %)))
          (map vec)
          shuffle
          (map #(scan-chunk % answer-1))
          (find-first some?)
          (apply +)))))

answer-2
;; => 62
;; => 3340942

