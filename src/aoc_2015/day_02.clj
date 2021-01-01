(ns aoc-2015.day-02
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [common.util :as util]))

(defn parse-present [s]
  (as-> s $
    (str/split $ #"x")
    (map edn/read-string $)))

;;
;; Part 1
;;

(defn paper-area [dims]
  (let [areas (map * dims (-> dims cycle rest))
        extra (apply min areas)]
    (+ extra (* 2 (reduce + 0 areas)))))

(def str->area (comp paper-area parse-present))

(->> "2015/day_02_input.txt"
     util/fetch-lines
     (map str->area)
     (reduce + 0))
;; => 1586300

;;
;; Part 2
;;

(defn ribbon-length [dims]
  (let [sides (butlast (sort dims))
        p     (* 2 (apply + sides))
        v     (apply * dims)]
    (+ p v)))

(def str->ribbon-length (comp ribbon-length parse-present))

(->> "2015/day_02_input.txt"
     util/fetch-lines
     (map str->ribbon-length)
     (reduce + 0))
;; => 3737498
