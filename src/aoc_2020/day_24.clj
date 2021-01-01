(ns aoc-2020.day-24
  (:require [common.util :as util]
            [clojure.set :as set]))

;; See https://www.redblobgames.com/grids/hexagons/#coordinates
;; for coordinate systems on hexagonal maps

(def directions
  {:w  [-1  0]
   :e  [+1  0]
   :nw [ 0 -1]
   :ne [+1 -1]
   :sw [-1 +1]
   :se [ 0 +1]})

(def add-tiles (partial mapv +))

(defn neighbours [t]
  (map (partial add-tiles t) (vals directions)))

(defn read-input [filename]
  (->> filename
       util/fetch-lines
       (map #(re-seq #"(e|w|se|sw|ne|nw)" %))
       (map #(map (comp directions keyword second) %))
       (map #(apply add-tiles %))
       frequencies
       (map (fn [[k v]] [k (mod v 2)]))))

(defn part-1 [filename]
  (->> filename
       read-input
       (map second)
       (apply +)))

(part-1 "2020/day_24_test.txt")
;; => 10

(part-1 "2020/day_24_input.txt")
;; => 424

(defn input->blacks [filename]
  (->> filename
       read-input
       (filter (fn [[_ v]] (= 1 v)))
       (map first)
       (into #{})))

(defn hood-cnt [blacks t]
  (->> t
       neighbours
       (filter blacks)
       count))

(defn step [blacks]
  (let [whites     (set/difference (set (mapcat neighbours blacks))
                                   blacks)
        new-blacks (set (filter #(= 2 (hood-cnt blacks %)) whites))
        new-whites (set (filter #((complement #{1 2}) (hood-cnt blacks %)) blacks))]
    (set/union new-blacks (set/difference blacks new-whites))))

(defn solve-part-2 [blacks n]
  (let [iters (iterate step blacks)
        res   (nth iters n)]
    (count res)))

(defn part-2 [filename]
  (-> filename
      input->blacks
      (solve-part-2 100)))

(time
 (def part-2-test
   (part-2 "2020/day_24_test.txt")))

part-2-test
;; => 2208

(time
 (def part-2-answer
   (part-2 "2020/day_24_input.txt")))

part-2-answer
;; => 3737
