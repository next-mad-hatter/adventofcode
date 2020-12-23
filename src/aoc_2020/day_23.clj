(ns aoc-2020.day-23
  (:require [aoc-2020.util :as util]
            [clojure.edn :as edn]
            [clojure.core.rrb-vector :as fv]
            [clojure.string :as str]
            [taoensso.tufte :as tufte :refer [p profile]]))

(defn read-input [s]
  (as-> s $
    (str/split $ #"")
    (map edn/read-string $)
    (apply fv/vector-of :long $)))

(defn decrease [e m not-es]
  (loop [e e
         cnt 0]
    (if (> cnt 42)
      nil
      (let [op #(inc (mod (- % 2) m))
            e' (op e)]
        (if (some #(= e' %) not-es) (recur e' (inc cnt))  e')))))

(= 4 (decrease 3 5 #{1 2 5}))
;; => true
(= 1 (decrease 2 5 #{4 3 2}))
;; => true
(= 5 (decrease 1 5 #{3 2 1}))
;; => true
(= nil (decrease 1 3 #{1 2 3}))
;; => true

(defn pos-of [x v]
  (first (keep-indexed (fn [i e] (when (= x e) i)) v)))

(defn move [m x]
  (let [e      (p ::e (first x))
        chunk  (p ::c (fv/subvec x 1 4))
        e'     (p ::d (decrease e m chunk))
        tail   (p ::t (fv/subvec x 4))
        pivot  (p ::p (inc (pos-of e' tail)))
        tail-l (p ::l (fv/subvec tail 0 pivot))
        tail-r (p ::r (fv/subvec tail pivot))]
    (p ::o (fv/catvec tail-l chunk (conj tail-r e)))))

(defn output [x]
  (let [pivot (pos-of 1 x)
        left  (fv/subvec x 0 pivot)
        right (fv/subvec x (inc pivot))]
    (apply str (fv/catvec right left))))

(defn part-1 [input]
  (let [ds (read-input input)
        f  (partial move (apply max ds))]
    (-> f
        (iterate ds)
        (nth 100)
        output)))

(time
 (part-1 "389125467"))
;; => "67384529"

(time
 (part-1 "974618352"))
;; => "75893264"

;;
;; scratchpad…
;;

(def input
  (read-input "389125467"))

(def m (apply max input))

(def start
  (fv/catvec
   input
   (apply fv/vector-of :long (range (inc m) 1000001))))

(def f (partial move 1000000))

(def is (iterate f start))

(tufte/add-basic-println-handler! {})

(time
 (profile
  {}
  (first
   (nth is 10))))
