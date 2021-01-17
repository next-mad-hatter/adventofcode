(ns aoc-2015.day-04
  (:require [digest :as digest]))

(defn find-number [z prefix]
  (loop [n 0]
    (let [in   (str prefix n)
          pass (->> in
                    digest/md5
                    (take z)
                    (apply = \0))]
      (if pass n (recur (inc n))) )))

(comment)
(time
 (find-number 5 "abcdef"))
;; => 609043

(time
 (find-number 5 "pqrstuv"))
;; => 1048970

(time
 (find-number 5 "bgvyzdsv"))
;; => 254575

(time
 (find-number 6 "bgvyzdsv"))
;; => 1038736
