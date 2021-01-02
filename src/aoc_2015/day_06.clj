(ns aoc-2015.day-06
  (:require [clojure.edn :as edn]
            [common.util :as util]))

(defn parse-line [s]
  (let [match (rest (re-matches #"(turn off|turn on|toggle) (\d+),(\d+) through (\d+),(\d+)" s))
        op    ({"turn on" :on "turn off" :off "toggle" :toggle} (first match))
        cs    (mapv edn/read-string (rest match))]
    [op cs]))

(defn update-in-state [state ind f]
  (assoc! state ind (f (state ind))))

(defn apply-to [op-map state [op [x1 y1 x2 y2]]]
  (->>
   (for [x (range x1 (inc x2))
         y (range y1 (inc y2))
         :let
         [ind (util/grid->flat 1000 x y)
          op  (op-map op)]]
     #(update-in-state % ind op))
   (reduce #(%2 %1) state)))

(def init-state (vec (repeat 1000000 0)))

(defn solve [op-map]
 (->> "2015/day_06_input.txt"
      util/fetch-lines
      (map parse-line)
      (reduce (partial apply-to op-map) (transient init-state))
      persistent!
      (reduce + 0)))
;;
;; Part 1
;;

(time
 (solve {:on     (constantly 1)
         :off    (constantly 0)
         :toggle #(- 1 %)}))
;; => 543903

;;
;; Part 2
;;

(time
 (solve {:on     inc
         :off    #(max 0 (dec %))
         :toggle #(+ 2 %)}))
;; => 14687245
