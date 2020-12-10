(ns aoc-2020.day-04
  (:require
   [aoc-2020.util :as util]
   [clojure.set :as set]))

(defn parse [p]
  (as-> p v
    (re-seq #"(\S+):(\S+)" v)
    (map #(into [] [(keyword (nth % 1)) (nth % 2)]) v)
    (into {} v)))

(time
 (def passes
   (->>
    "2020/day_04_input.txt"
    (util/fetch-chunks #"\R\s*\R")
    (map parse))))

(defn valid [p]
  (and
   (set/subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid} (into #{} (keys p)))
   (re-matches #"\d{4}" (:byr p))
   (<= 1920 (read-string (:byr p)) 2002)
   (re-matches #"\d{4}" (:eyr p))
   (<= 2020 (read-string (:eyr p)) 2030)
   (re-matches #"\d{4}" (:iyr p))
   (<= 2010 (read-string (:iyr p)) 2020)
   (re-matches #"\d{9}" (:pid p))
   (contains? #{:amb :blu :brn :gry :grn :hzl :oth} (keyword (:ecl p)))
   (re-matches #"#([\da-f]{6})" (:hcl p))
   (re-matches #"\d+(cm|in)" (:hgt p))
   (let [m     (re-matches #"(\d+)(cm|in)"  (:hgt p))
         val   (read-string (nth m 1))
         units (nth m 2)]
     (if (= "cm" units)
       (<= 150 val 193)
       (<= 59 val 76)))))

(time
 (count (filter valid passes)))
