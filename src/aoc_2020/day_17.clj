(ns aoc-2020.day-17
  (:require [common.util :as util]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn pad-v [dim v]
  (into v (repeat (- dim (count v)) 0)))

(defn row->points [dim x row]
  (->> row
       (combo/cartesian-product (vector x))
       (map (partial apply (fn [x [y v]] [(pad-v dim [x y]) v])))))

(defn read-input [dim filename]
  (->>
   filename
   util/fetch-lines
   (map #(map-indexed (fn [k v] [k ({\. 0 \# 1} v)]) %))
   (map-indexed (partial row->points dim))
   (reduce into {})
   (filter (fn [[_ v]] (not= 0 v)))
   (map first)
   set))

(defn all-dirs [dim]
  (->>
   dim
   (combo/selections [-1 0 1])
   (filter #(not= #{0} (set %)))
   (mapv vec)))

(def add (partial mapv +))

(defn neighbours [dirs pt]
  (mapv #(add pt %) dirs))

(defn a-n-cnt [dirs active p]
  (->> p
       (neighbours dirs)
       (filter active)
       count))

(defn step [dirs active]
  (let [inactive   (set/difference (set (mapcat #(neighbours dirs %) active)) active)
        old-active (filter #(#{2 3} (a-n-cnt dirs active %)) active)
        new-active (filter #(= 3 (a-n-cnt dirs active %)) inactive)]
    (set/union (set old-active) (set new-active))))

(defn solve [dim filename]
  (->> filename
       (read-input dim)
       (iterate (partial step (all-dirs dim)))
       (drop 6)
       first
       count))

(solve 3 "2020/day_17_test.txt")
;; => 112

(time
 (solve 3 "2020/day_17_input.txt"))
;; => 359

(time
 (solve 4 "2020/day_17_test.txt"))
;; => 848

(time
 (solve 4 "2020/day_17_input.txt"))
;; => 2228
