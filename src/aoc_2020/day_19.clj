(ns aoc-2020.day-19
  (:require [aoc-2020.util :as util]
            [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(defn read-rule [rule]
  (str/replace rule #"(\d+)" "rule-$1"))

(defn replace-rule [rule]
  (match rule
         "8: 42"     "8: 42 | 42 8"
         "11: 42 31" "11: 42 31 | 42 11 31"
         :else rule))

(defn get-input [filename tr]
  (let [chunks         (util/fetch-chunks #"\R\s*\R" filename)
        get-input-part (fn [n] (-> chunks (get n) str/split-lines vec))
        words          (get-input-part 1)
        rules          (->> (get-input-part 0) (map tr) (map read-rule))]
    {:grammar (str/join ";\n" rules) :words words}))

(-> "2020/day_19_test_2.txt" (get-input replace-rule))

(defn tester [grammar]
  (insta/parser grammar :start :rule-0))

(defn solver [tr]
  (fn [filename]
    (let [data   (get-input filename tr)
          tester (tester (data :grammar))
          words  (data :words)]
      (count (remove insta/failure? (map tester words))))))

(time
 ((solver identity) "2020/day_19_input.txt"))
;; => 216

(time
 ((solver replace-rule) "2020/day_19_input.txt"))
;; => 400
