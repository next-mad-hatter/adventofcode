(ns aoc-2020.day-02
  (:require [common.util :as util]
            [clojure.algo.generic.functor :as gf :refer [fmap]]))

(defn match->entry [matches]
  (gf/fmap #(nth matches %) {:min 1 :max 2 :char 3 :pw 4}))

(defn input->entries [input]
  (as-> input v
    (util/fetch-lines v)
    (map #(re-matches #"(\d+)-(\d+)\s+(.):\s+(.*)" %) v)
    (map match->entry v)))

;; Part 1

(defn sat-one [entry]
  (let [c (first (:char entry))
        f (count (filter #{c} (seq (:pw entry))))]
    (<= (read-string (:min entry)) f (read-string (:max entry)))))

(util/spy
 (time
  (->>
   "2020/day_02_input.txt"
   input->entries
   (filter sat-one)
   count)))
;; => 454

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

(util/spy
 (time
  (->>
   "2020/day_02_input.txt"
   input->entries
   (filter sat-two)
   count)))
;; => 649
