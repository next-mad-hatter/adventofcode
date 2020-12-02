(ns madhat.adventofcode.day-one
  (:require [clojure.string :as str])
  (:require [clojure.math.combinatorics :as combo]))


(def input (slurp "day_1_input.txt"))


(def input-seq
  (as-> input v
    (str/split v #"\s+")
    (filter #(not (= "" %)) v)
    (map read-string v)))


;; set combinations length to 2 or 3 depending on task part
(as-> input-seq v
  (combo/combinations v 3)
  (filter #(= 2020 (apply + %)) v)
  (first v)
  (apply * v))


;; TODO: map cider-pprint-eval-last-sexp-to-comment in doom
;; => 130933530
