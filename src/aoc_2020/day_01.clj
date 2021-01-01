(ns aoc-2020.day-01
  (:require [common.util :as util]
            [clojure.math.combinatorics :as combo]))

(defn solve [input n]
  (as-> input v
    (combo/combinations v n)
    (filter #(= 2020 (apply + %)) v)
    (first v)
    (apply * v)))

(def input (util/fetch-numbers "2020/day_01_input.txt"))

(time
 (map (partial solve input) [2 3]))
;; => (888331 130933530)
