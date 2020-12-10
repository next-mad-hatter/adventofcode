(ns aoc-2020.day-06
  (:require
   [aoc-2020.util :as util]
   [clojure.string :as str]
   [clojure.set :as set]))

(defn parse-1 [p]
  (->> p
    set
    (remove #{\newline \return})))

(defn parse-2 [p]
  (->> p
    str/split-lines
    (map set)
    (apply set/intersection)))

(defn solver [parser]
  (fn [input]
     (->> input
          (util/fetch-chunks #"\R\s*\R")
          (map parser)
          (map count)
          (apply +))))

(time
 (as-> [parse-1 parse-2] $
   (map solver $)
   ((partial apply juxt) $)
   ($ "2020/day_06_input.txt")))
;; => [6778 3406]
