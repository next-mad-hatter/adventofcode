(ns aoc-2020.day-18
  (:require [common.util :as util]
            [instaparse.core :as insta]
            [clojure.edn :as edn]))

(def eval-tree
  (partial
   insta/transform
   {:add +, :mul *, :sub -, :div /, :number edn/read-string}))

(def parser-1
  (insta/parser
   "<expr> = (number | <'('> expr <')'> | add | mul )
    add = expr <'+'> expr
    mul = expr <'*'> expr
    number = #'[0-9]+'"
   :auto-whitespace :standard))

(def calc-1 (comp first eval-tree parser-1))

(def vis-1 (comp insta/visualize first parser-1))

;; Part 1

(time
 (->> "2020/day_18_input.txt"
      util/fetch-lines
      (map calc-1)
      (reduce + 0)))
;; => 8929569623593

;; Part 2

(def parser-2
  (insta/parser
   "<expr> = mul
    mul = add <'*'> expr | add
    add = term <'+'> add | term
    <term> = number | <'('> expr <')'>
    number = #'[0-9]+'"
   :auto-whitespace :standard))

(def calc-2 (comp first eval-tree parser-2))

(def vis-2 (comp insta/visualize first parser-2))

(time
 (->> "2020/day_18_input.txt"
      util/fetch-lines
      (map calc-2)
      (reduce + 0)))
;; => 231235959382961

