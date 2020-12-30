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
         (for [[algs, ings] input, a algs] {a (set ings)})))

(defn translate [inclusions]
  (loop [dictionary {}
         inclusions inclusions]
    (cond
      (some (comp empty? second) inclusions) nil
      (empty? inclusions)                    dictionary
      :else
      (let [singletons   (filter (comp #(= (count %) 1) second) inclusions)
            new-pairs    (map (juxt first (comp first second)) singletons)
            dictionary'  (into dictionary new-pairs)
            inclusions'  (apply dissoc inclusions (map first new-pairs))
            inclusions'' (fmap #(apply disj % (map second new-pairs)) inclusions')]
        (recur dictionary' inclusions'')))))

(defn solve-part-1 [input]
  (let [translated (-> input input->inclusions translate vals set)
        words      (mapcat second input)]
    (count (remove translated words))))

(defn solve-part-2 [input]
  (->> input
       input->inclusions
       translate
       sort
       (map second)
       (str/join ",")))

(def input
  (->> "2020/day_21_input.txt"
       read-input))

(solve-part-1 input)
;; => 2542

(solve-part-2 input)
;; => "hkflr,ctmcqjf,bfrq,srxphcm,snmxl,zvx,bd,mqvk"
