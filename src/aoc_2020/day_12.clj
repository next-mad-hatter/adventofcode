(ns aoc-2020.day-12
  (:require
   [aoc-2020.util :as util]
   [clojure.string :as str]
   [clojure.edn :as edn]
   #_[clojure.test :as t]))

(defn parse-line [line]
  (->> line
       (str/upper-case)
       (re-matches #"([NSEWLRF])(\d+)")
       rest
       ((juxt #(-> % first keyword) #(-> % second edn/read-string)))))

(defn rotate [dir rot-dir val]
  (mod
   (+ dir
      (* ({:L 1 :R -1} rot-dir) (/ val 90))) 4))

(defn move [pos dir val]
  (mapv + pos (map #(* val %) dir)))

(defn step [state [op val]]
  (let [dir-ops {:E 0 :N 1 :W 2 :S 3}
        dir->delta [[1 0] [0 1] [-1 0] [0 -1]]]
    (cond
      (contains? dir-ops op)
      (update state :pos #(move % (dir->delta (dir-ops op)) val))

      (#{:L :R} op)
      (update state :dir #(rotate % op val))

      (= :F op)
      (update state :pos #(move % (dir->delta (:dir state)) val))

      :else (throw (Exception. "Not implemented")))))

(def init {:pos [0 0] :dir 0})

(defn part-1 [filename]
  (->> filename
       util/fetch-lines
       (map parse-line)
       (reduce step init)
       :pos
       (map #(Math/abs %))
       (apply +)))

(part-1 "2020/day_12_test.txt")
;; => 25

(part-1 "2020/day_12_input.txt")
;; => 1603

;;
;; TODO: Part 2
;;
