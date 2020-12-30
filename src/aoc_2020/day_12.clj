(ns aoc-2020.day-12
  (:require [aoc-2020.util :as util]
            [clojure.string :as str]
            [clojure.edn :as edn]))

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

(defn step-one [state [op val]]
  (let [dir-ops    {:E 0 :N 1 :W 2 :S 3}
        dir->delta [[1 0] [0 1] [-1 0] [0 -1]]]
    (cond
      (contains? dir-ops op)
      (update state :pos #(move % (dir->delta (dir-ops op)) val))

      (#{:L :R} op)
      (update state :dir #(rotate % op val))

      (= :F op)
      (update state :pos #(move % (dir->delta (:dir state)) val))

      :else (throw (Exception. "Not implemented")))))

(def init-one {:pos [0 0] :dir 0})

(defn part-1 [filename]
  (->> filename
       util/fetch-lines
       (map parse-line)
       (reduce step-one init-one)
       :pos
       (map #(Math/abs %))
       (apply +)))

(part-1 "2020/day_12_test.txt")
;; => 25

(time
 (part-1 "2020/day_12_input.txt"))
;; => 1603

(def init-two {:pos [0 0] :dir [10 1]})

(defn rotate-vec [[x y] n]
  ([[x y] [(- y) x] [(- x) (- y)] [y (- x)]] n)
  ;; (let [rot (mat/matrix [[0 -1]
  ;;                        [1 0]])
  ;;       op (fn [[x y]] (mapv int (flatten (mat/mmul rot (mat/column-matrix [x y])))))
  ;;       its (iterate op [x y])
  ;;       ]
  ;;   (nth its n))
  )

(defn step-two [state [op val]]
  (let [dir-ops    {:E 0 :N 1 :W 2 :S 3}
        dir->delta [[1 0] [0 1] [-1 0] [0 -1]]]
    (cond
      (contains? dir-ops op)
      (update state :dir #(move % (dir->delta (dir-ops op)) val))

      (#{:L :R} op)
      (update state :dir #(rotate-vec % (rotate 0 op val)))

      (= :F op)
      (update state :pos #(move % (:dir state) val))

      :else (throw (Exception. "Not implemented")))))

(defn part-2 [filename]
  (->> filename
       util/fetch-lines
       (map parse-line)
       (reduce step-two init-two)
       :pos
       (map #(Math/abs %))
       (apply +)))

(part-2 "2020/day_12_test.txt")
;; => 286

(time
 (part-2 "2020/day_12_input.txt"))
;; => 52866
