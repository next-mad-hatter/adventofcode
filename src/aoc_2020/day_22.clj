(ns aoc-2020.day-22
  (:require [common.util :as util]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-chunk [s]
  (let [lines (str/split-lines s)
        name  (second (re-matches #"(Player (\d+)):.*" (first lines)))
        cards (map edn/read-string (rest lines))
        hand  (reduce conj clojure.lang.PersistentQueue/EMPTY cards)]
    [name hand]))

(defn read-input [filename]
  (->> filename
       (util/fetch-chunks #"\R\s*\R")
       (map parse-chunk)))

(defn score [q]
  (->> q
       reverse
       (map-indexed #(* (inc %1) %2))
       (reduce +)))

(defn step-1 [[q1 q2]]
  (cond (empty? q1) [0 (score q2)]
        (empty? q2) [(score q1) 0]
        :else
        (let [[x1 x2] (mapv peek [q1 q2])
              add-to  #(reduce conj %1 [%2 %3])
              [q1 q2] (mapv pop [q1 q2])]
          (if (> x1 x2)
            [(add-to q1 x1 x2) q2]
            [q1 (add-to q2 x2 x1)]))))

(defn part-1 [filename]
  (->> filename
       read-input
       (map second)
       (iterate step-1)
       (util/find-first (comp number? first))
       (apply max)))

(part-1 "2020/day_22_test.txt")
;; => 306

(time
 (part-1 "2020/day_22_input.txt"))
;; => 32629

(defn enqueue [n q]
  (->> q (take n) (into clojure.lang.PersistentQueue/EMPTY)))

(defn play-2 [q1 q2]
  (loop [seen #{}
         q1   q1
         q2   q2]
    (cond
      (seen [q1 q2]) [0 -1]
      (empty? q1)    [0 (score q2)]
      (empty? q2)    [(score q1) 0]
      :else
      (let [seen'    (conj seen [q1 q2])
            add-to   #(reduce conj %1 [%2 %3])
            [x1 x2]  (mapv peek  [q1 q2])
            [l1 l2]  (mapv count [q1 q2])
            [q1 q2]  (mapv pop   [q1 q2])
            p-1-wins (if (or (>= x1 l1) (>= x2 l2))
                       (> x1 x2)
                       (let [[s1 s2] (play-2 (enqueue x1 q1) (enqueue x2 q2))]
                         (> s1 s2)))]
        (if p-1-wins
          (recur seen' (add-to q1 x1 x2) q2)
          (recur seen' q1 (add-to q2 x2 x1)))))))

(defn part-2 [filename]
  (->> filename
       read-input
       (map second)
       (apply play-2)
       (apply max)))

(part-2 "2020/day_22_test.txt")
;; => 291

(time
 (part-2 "2020/day_22_input.txt"))
;; => 32519
