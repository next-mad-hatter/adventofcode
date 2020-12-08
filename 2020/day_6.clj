(ns madhat.adventofcode.day-six
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

(defn split-input [input]
  (str/split input #"\R\s*\R"))

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
      slurp
      split-input
      (map parser)
      (map count)
      (apply +))))

(time
 (as-> [parse-1 parse-2] $
   (map solver $)
   ((partial apply juxt) $)
   ($ "day_6_input.txt")))
;; => [6778 3406]
