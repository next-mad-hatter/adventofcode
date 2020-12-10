(ns aoc-2020.day-10
  (:require
   [aoc-2020.util :as util]))

;; Part 1

(defn complete-path [path vertices]
  (loop [path path vertices vertices]
    (if (empty? vertices)
      path
      (let [candidates (filter #(<= (- % 3) (last path) %) vertices)]
        (when-not (empty? candidates)
          ;; Since our adaptors only accept joltage no higher than their rating,
          ;; we only need to consider the lowest rated candidate here.
          (let [v        (apply min candidates)
                path     (conj path v)
                vertices (disj vertices v)]
            (recur path vertices)))))))

(defn solve-with [init solver input]
  (let [sink (+ 3 (apply max input))
        adaptors (set input)]
    (if (not= (count input) (count adaptors))
      (throw (Exception. "Not implemented"))
      (solver init (conj adaptors sink)))))

(defn filename->answer-1 [filename]
  (->> filename
       util/fetch-numbers
       (solve-with [0] complete-path)
       (partition 2 1)
       (map (partial apply -))
       frequencies
       vals
       (apply *)))

(defn part-1 []
  (map filename->answer-1
       ["2020/day_10_test_1.txt" "2020/day_10_test_2.txt" "2020/day_10_input.txt"]))

(time
  (part-1))
;; => (35 220 2030)

;;  Part 2

(def path-counter
  (memoize
   (fn [src vertices]
     (if (empty? vertices)
       1
       (let [candidates (filter #(<= (- % 3) src %) vertices)
             subpaths   (map (fn [v] (filter #(> % v) vertices)) candidates)
             solutions  (map path-counter candidates subpaths)]
         (apply + solutions))))))

(defn part-2 []
  (time
   (map #(->> % util/fetch-numbers (solve-with 0 path-counter))
        ["2020/day_10_test_1.txt" "2020/day_10_test_2.txt" "2020/day_10_input.txt"])))

(time
 (part-2))
;; => (8 19208 42313823813632)
