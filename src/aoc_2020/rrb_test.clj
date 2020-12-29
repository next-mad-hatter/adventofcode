(ns rrb-test
  (:require [clojure.core.rrb-vector :as fv]))

(let [v (vector 1 2 3)
      t (transient v)]
  [(v 1) (t 1)])
;; => [2 2]

(let [v (fv/vector 1 2 3)
      t (transient v)]
  [(v 1) (t 1)])
;; => [2 nil]

(let [v (into (fv/vector-of :int 0) (map inc (range 42)))
      v'  (assoc v 40 5)
      v'' (assoc v' 5 40)]
  [(get v'' 40) v''])
