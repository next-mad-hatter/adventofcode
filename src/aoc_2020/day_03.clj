(ns aoc-2020.day-03
  (:require [common.util :as util]))

(defn find-trees-in
  "Given a line, returns the set of zero-based indices of its tree locations"
  [line]
  (->> line
       (keep-indexed #(when (= \# %2) %1))
       (into #{})))

(defn load-topo [filename]
  (let [lines (util/fetch-lines filename)]
    {:width (count (first lines))
     :lines (map find-trees-in lines)}))

(defn sled
  "Returns number of trees encountered when moving through given topography along given vector"
  [dx dy topo]
  (let [width     (:width topo)
        lines     (take-nth dy (:lines topo))
        modmul    (fn [t] (mod (* t dx) width))
        hits-tree (fn [t line] (contains? line (modmul t)))]
    (->> (map-indexed hits-tree lines)
         (frequencies)
         (#(get % true)))))

;; Part 1

(def topo (load-topo "2020/day_03_input.txt"))

(time
 (sled 3 1 topo))
;; => 280

;; Part 2

(def slopes [[1 1]
             [3 1]
             [5 1]
             [7 1]
             [1 2]])

(time
 (letfn [(count-trees [dx dy] (sled dx dy topo))]
   (->> slopes
        (map (partial apply count-trees))
        (apply *))))
;; => 4355551200
