(ns aoc-2020.day-19
  (:require
   [aoc-2020.util :as util]
   [instaparse.core :as insta]
   [clojure.core.match :refer [match]]
   [clojure.string :as str]))

(def directions
  {:w  [-1  0]
   :e  [+1  0]
   :nw [ 0 -1]
   :ne [+1 -1]
   :sw [-1 +1]
   :se [ 0  +1]})

(defn read-input [filename]
  (->> filename
       util/fetch-lines
       (map #(re-seq #"(e|w|se|sw|ne|nw)" %))
       (map #(map (comp directions keyword second) %))
       (map #(apply (partial mapv +) %))
       frequencies
       (map (fn [[_ v]] (mod v 2)))
       (apply +)))

(read-input "2020/day_24_test.txt")
;; => 10

(read-input "2020/day_24_input.txt")
;; => 424
