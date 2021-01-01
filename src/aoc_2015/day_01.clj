(ns aoc-2015.day-01
  (:require
   [common.util :as util]))

(defn parse [s]
  (->> s
       (map {\( +1 \) -1})))

(defn guard [xs]
  (loop [p 1, s 0, xs xs]
    (let [x   (first xs)
          xs' (rest xs)
          s'  (+ s x)]
      (if (< s' 0) p
          (recur (inc p) s' xs')))))

(->> "2015/day_01_input.txt"
     util/fetch-whole
     parse
     (reduce +))
;; => 138

(->> "2015/day_01_input.txt"
     util/fetch-whole
     parse
     guard)
;; => 1771

