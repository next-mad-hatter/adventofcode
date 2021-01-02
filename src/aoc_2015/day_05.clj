(ns aoc-2015.day-05
  (:require [common.util :as util]))

(defn nice-1? [s]
  (and
   (some? (re-find #"(.)\1" s))
   (nil? (re-find #"ab|cd|pq|xy" s))
   (->> s
        (filter #{\a \e \i\ \o \u})
        count
        (< 2))))

(map nice-1? ["ugknbfddgicrmopn" "aaa" "jchzalrnumimnmhp" "haegwjzuvuyypxyu" "dvszwmarrgswjxmb"])
;; => (true true false false false)

(defn nice-2? [s]
  (and
   (some? (re-find #"(.).\1" s))
   (some? (re-find #"(..).*\1" s))))

(map nice-2? ["qjhvhtzxzqqjkmpb" "xxyxx" "uurcxstgmygtbstg" "ieodomkazucvgmuy"])
;; => (true true false false)

(->> "2015/day_05_input.txt"
     util/fetch-lines
     (filter nice-1?)
     count)
;; => 255

(->> "2015/day_05_input.txt"
     util/fetch-lines
     (filter nice-2?)
     count)
;; => 55
