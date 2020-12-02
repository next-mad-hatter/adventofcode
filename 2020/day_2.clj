(ns madhat.adventofcode.day-two
  (:require [clojure.string :as str]
            [clojure.algo.generic.functor :as gf :only (fmap)]
            [clojure.math.combinatorics :as combo]))


(defn match->entry [matches]
  (gf/fmap #(nth matches %) {:min 1 :max 2 :char 3 :pw 4}))


(defn input->entries [input]
  (as-> input v
    (str/split v #"\n+")
    (map #(re-matches #"(\d+)-(\d+)\s+(.):\s+(.*)" %) v)
    (map match->entry v)))


;; Part 1

(defn sat-one [entry]
  (let [c (first (:char entry))
        f (count (filter #{c} (seq (:pw entry))))]
    (<= (read-string (:min entry)) f (read-string (:max entry)))))


(->>
 "day_2_input.txt"
 (slurp)
 (input->entries)
 (filter sat-one)
 (count))


;; Part 2

(defn sat-two [entry]
  (let [ind-one (dec (read-string (:min entry)))
        ind-two (dec (read-string (:max entry)))
        chars   (seq (:pw entry))
        one     (nth chars ind-one)
        two     (nth chars ind-two)
        c       (first (:char entry))
        f       (count (filter  #{c} [one two]))]
    (= f 1)))


(->>
 "day_2_input.txt"
 (slurp)
 (input->entries)
 (filter sat-two)
 (count))
