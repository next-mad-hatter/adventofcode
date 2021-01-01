(ns aoc-2015.day-03
  (:require [clojure.set :as set]
            [common.util :as util]))

(defn parse-dir [s] s
  (map {\^ [0 -1]
        \v [0 +1]
        \< [-1 0]
        \> [+1 0]} s))

(defn step [[visited pos] dir]
  (let [pos' (mapv + pos dir)]
    [(conj visited pos') pos']))

(defn exec [s]
  (->> s
       parse-dir
       (reduce step [#{[0 0]} [0 0]])))
;;
;; Part 1
;;

(->> "2015/day_03_input.txt"
     util/fetch-whole
     exec
     first
     count)
;; => 2081

;;
;; Part 2
;;

(->> "2015/day_03_input.txt"
     util/fetch-whole
     util/split-in-two
     (map exec)
     (map first)
     (apply set/union)
     count)
;; => 2341
