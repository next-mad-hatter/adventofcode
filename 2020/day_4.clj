(ns madhat.adventofcode.day-three
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(defn input->passports [input]
  (str/split input #"\n\s*\n"))

(defn parse [p]
  (as-> p v
    (re-seq #"(\S+):(\S+)" v)
    (map #(into [] [(keyword (nth % 1)) (nth % 2)]) v)
    (into {} v)))

(def passes
  (->>
   "day_4_input.txt"
   slurp
   input->passports
   (map parse)))

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

passes

(count (filter valid passes))

;; Part 2
