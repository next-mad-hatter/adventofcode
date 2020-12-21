(ns aoc-2020.day-21
  (:require [aoc-2020.util :as util]
            [clojure.set :as set]
            [clojure.algo.generic.functor :refer [fmap]]
            [clojure.string :as str]))

(defn read-input [filename]
  (->> filename
       util/fetch-lines
       (map #(re-matches #"(.+)\s+\(contains\s+(.+)\)\s*" %))
       (map rest)
       (map (juxt #(-> % second (str/split #",\s*"))
                  #(-> % first (str/split #"\s+"))))))

(defn input->inclusions [input]
  (apply merge-with set/intersection
         (for [[algs, ings] input, a algs] {a , (set ings)})))

(defn translate [inclusions]
  (loop [dictionary {}
         inclusions inclusions]
    (cond
      (some (comp empty? second) inclusions) nil
      (empty? inclusions) dictionary
      :else
      (let [singletons   (filter (comp #(= (count %) 1) second) inclusions)
            new-dict     (map (juxt first (comp first second)) singletons)
            dictionary'  (into dictionary new-dict)
            inclusions'  (apply dissoc inclusions (map first new-dict))
            inclusions'' (fmap #(apply disj % (map second new-dict)) inclusions')]
        (recur dictionary' inclusions'')))))

(defn solve-part-1 [input]
  (let [translated (set (vals (translate (input->inclusions input))))
        words      (mapcat second input)]
    (count (remove translated words))))

(def input
  (->> "2020/day_21_input.txt"
       read-input))

(solve-part-1 input)
;; => 2542

(defn solve-part-2 [input]
  (->> input
       input->inclusions
       translate
       sort
       (map second)
       (str/join ",")))

(solve-part-2 input)
;; => "hkflr,ctmcqjf,bfrq,srxphcm,snmxl,zvx,bd,mqvk"
