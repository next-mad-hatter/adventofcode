(ns aoc-2020.day-16
  (:require
   [aoc-2020.util :as util]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [clojure.algo.generic.functor :refer [fmap]]
   [clojure.set :as set]))

(defn parse-domain [r]
  (let [ms (rest (re-matches #"(.+)\s*:\s+(\d+)\s*-\s*(\d+)\s+or\s+(\d+)\s*-\s*(\d+)" r))]
    [(first ms) (mapv edn/read-string (rest ms))]))

(defn parse-ticket [t]
  (mapv edn/read-string (str/split t #",")))

(defn read-input [filename]
  (let [chunks (util/fetch-chunks #"\R\s*\R" filename)
        ranges (str/split-lines (first chunks))
        mine   (first (rest (str/split-lines (second chunks))))
        others (rest (str/split-lines (nth chunks 2)))]
    {:domains (into {} (map parse-domain ranges)) :mine (parse-ticket mine) :others (mapv parse-ticket others)}))

(defn in-domain [[a b c d] x]
  (or (<= a x b) (<= c x d)))

(defn in-some [domains]
  (let [fs (map #(partial in-domain %) domains)]
    #(reduce (fn [x y] (or x y)) (map (fn [f] (f %)) fs))))

(defn part-1 [filename]
  (let [input (read-input filename)]
    (apply + (remove #((in-some (vals (:domains input))) %) (flatten (:others input))))))

(part-1 "2020/day_16_test_1.txt")
;; => 71

(time
 (part-1 "2020/day_16_input.txt"))
;; => 25972

;; TODO: Part 2

(defn set-in-domain [d s]
  (every? #(in-domain d %) s))

(defn comp-domains [ds]
  (fn [s]
    (->> ds
         (filter (fn [[_ v]] (set-in-domain v s)))
         (map first)
         set)))

(defn transpose [x]
  (apply mapv vector x))

(defn tickets->sets [ts]
  (map set (transpose ts)))

(defn translate [possible]
  (loop [dictionary {}
         possible possible]
    (cond
      (some (comp empty? second) possible) nil
      (empty? possible) (set/map-invert dictionary)
      :else
      (let [singletons   (filter (comp #(= (count %) 1) second) possible)
            new-pairs    (map (juxt first (comp first second)) singletons)
            dictionary'  (into dictionary new-pairs)
            possible'  (apply dissoc possible (map first new-pairs))
            possible'' (fmap #(apply disj % (map second new-pairs)) possible')]
        (recur dictionary' possible'')))))

(defn solve [input]
  (let [ds       (:domains input)
        valid    (filter #(every? (in-some (vals ds)) %) (:others input))
        ts       (tickets->sets valid)
        possible (into {} (map-indexed vector (map (comp-domains ds) ts)))]
    (translate possible)))

(defn part-2 [filename]
  (->> filename
       read-input
       solve))

(part-2 "2020/day_16_test_1.txt")

(part-2 "2020/day_16_test_2.txt")

(defn answer-2 [filename]
  (let [input   (read-input filename)
        t       (:mine input)
        indices (->> input
                     solve
                     (filter (fn [[k _]] (re-matches #"departure.*" k)))
                     (map second))
        vals    (map #(nth t %) indices)]
    (apply * vals)))

(time
 (answer-2 "2020/day_16_input.txt"))
;; => 622670335901
