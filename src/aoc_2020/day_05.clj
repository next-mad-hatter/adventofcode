(ns aoc-2020.day-05
  (:require [common.util :as util]))

;;
;; Another possibility for decoding:
;; (str/escape "FLBR" {\F 0 \L 0 \B 1 \R 1})
;;
;; Also, there's actually no need in these tasks
;; to split the numbers into the upper/lower parts
;;
(defn parse [p]
  (as-> p $
    ((partial map {\F 0 \B 1 \L 0 \R 1}) $)
    (apply str $)
    (Integer/parseInt $ 2)
    ((juxt #(-> % (/ 8) int) #(mod % 8)) $)))

(time
 (def nums
   (->>
    "2020/day_05_input.txt"
    util/fetch-lines
    (map parse)
    (into #{}))))

(time
 (def part-1-answer
   (apply max (map (partial apply #(+ (* 8 %1) %2)) nums))))

part-1-answer ;; => 959

(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))

(time
 (def part-2-answer
   (apply #(+ (* 8 %1) %2)
          (first
           (for [x     (range 256)
                 y     (range 8)
                 :when (not (contains? nums [x y]))
                 :when (contains? nums [(dec x) y])
                 :when (contains? nums [(inc x) y])]
             [x y])))))

part-2-answer ;; => 527
