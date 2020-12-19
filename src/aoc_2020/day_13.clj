(ns aoc-2020.day-13
  (:require
   [aoc-2020.util :as util]
   [clojure.string :as str]
   [clojure.edn :as edn]))

(defn get-input-1 [filename]
  (let [lines (util/fetch-lines filename)
        stamp (first lines)
        factors (remove #{"x"} (str/split (second lines) #","))]
    {:stamp (edn/read-string stamp) :factors (mapv edn/read-string factors)}))

(defn part-1 [filename]
  (->> filename
       get-input-1
       (#(map (juxt identity (fn [x] (mod (:stamp %) x))) (:factors %)))
       (map (juxt first (partial apply -)))
       (apply min-key second)
       (apply *)))

(time
 (part-1 "2020/day_13_test.txt"))
;; => 295

(time
 (part-1 "2020/day_13_input.txt"))
;; => 2845

(defn parse-string-2 [input]
  (let [positioned (str/split input #",")
        pairs (keep-indexed #(when (not= %2 "x") [%1 (edn/read-string %2)]) positioned)]
    (map (fn [[a n]] [(mod (- a) n) n]) pairs)))

(defn get-input-2 [filename]
  (->> filename
       util/fetch-lines
       second))

(defn xgcd [a b]
  (loop [[r' r] [a b]
         [s' s] [1 0]
         [t' t] [0 1]]
    (if (= r 0)
      [r' s' t']
      (let [q (biginteger (/ r' r))
            [r' r] [r (- r' (* r q))]
            [s' s] [s (- s' (* s q))]
            [t' t] [t (- t' (* t q))]]
        (recur [r' r] [s' s] [t' t])))))

(defn solve [[a' n'] [a'' n'']]
  (let [[g m' m''] (xgcd n' n'')
        f (bigint (* n' n''))]
    (when (not= 1 g) (throw (Exception. "Not coprime")))
    [(mod (+ (* a' m'' n'') (* a'' m' n')) f)
     f]))

(defn solve-string [input]
  (->> input
       parse-string-2
       (reduce solve)
       first))

(time (solve-string (get-input-2 "2020/day_13_test.txt")))
;; => 1068781N

(time (solve-string (get-input-2 "2020/day_13_input.txt")))
;; => 487905974205117N
