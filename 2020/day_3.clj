(ns madhat.adventofcode.day-three
  (:require [clojure.string :as str]))


(defn find-trees-in
  "Given a line, returns the set of zero-based indices of its tree locations"
  [line]
  (->> line
       (keep-indexed #(when (= \# %2) %1))
       (into #{})))

(defn input->topo [input]
  (let [lines (str/split input #"\n+")]
    {:width  (count (first lines))
     :height (count lines)
     :lines  (map find-trees-in lines)}))


(defn move
  "Returns number of trees encountered when moving through given topography along given vector"
  [dx dy topo]
  (loop [x         0
         y         0
         trees-cnt 0]
    (if (>= y (:height topo))
      trees-cnt
      (let [trees-coors (-> topo (:lines) (nth y))
            new-cnt     (if (trees-coors (mod x (:width topo))) (inc trees-cnt) trees-cnt)]
        (recur (+ x dx) (+ y dy) new-cnt)))))


;; Part 1

(def topo
  (->>
   "day_3_input.txt"
   (slurp)
   (input->topo)))

(move 3 1 topo) ;; => 280


;; Part 2

(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn count-trees [dx dy] (move dx dy topo))

(apply * (map (partial apply count-trees) slopes)) ;; => 4355551200
