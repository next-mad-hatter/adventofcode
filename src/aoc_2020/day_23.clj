(ns aoc-2020.day-23
  (:require [clojure.edn :as edn]
            [clojure.core.rrb-vector :as fv]
            [clojure.string :as str]
            [clojure.core.reducers :as r]))

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

(defn pad-input [input len]
  (let [m (apply max input)]
    ;; TODO: assert that input contains elements 1..m with m > 8
    (fv/catvec
     input
     (apply fv/vector-of :long (range (inc m) (inc len))))))

(defn initialize
  "Creates lookup vector: val -> [previous-val val next-val]"
  [input]
  (let [n (count input)]
    (into [nil]            ; we don't want to deal with 0-based lookup right now
          (sort-by second
                   (for [i (range n)]
                     (let [i- (mod (dec i) n)
                           i+ (mod (inc i) n)]
                       (mapv input (vector-of :long i- i i+))))))))

(defn prev-val [lut n] (first (lut n)))

(defn next-val [lut n] (last (lut n)))

(defn move-lut [[head lut]]
  (let [x head
        a (next-val lut x)
        b (next-val lut a)
        c (next-val lut b)
        y (next-val lut c)
        l (decrease x (dec (count lut)) #{a b c})
        r (next-val lut l)
        ;; NB: l might be equal to y, r -- to x, hence the reduction
        ops [#(assoc % x (let [[p v _] (% x)] [p v y]))
             #(assoc % y (let [[_ v n] (% y)] [x v n]))

             #(assoc % c (let [[p v _] (% c)] [p v r]))
             #(assoc % r (let [[_ v n] (% r)] [c v n]))

             #(assoc % l (let [[p v _] (% l)] [p v a]))
             #(assoc % a (let [[_ v n] (% a)] [l v n]))]
        lut' (r/reduce #(%2 %1)  lut ops)]
    [(next-val lut' head) lut']))

(defn output-1 [lut]
  (let [iters (iterate #(next-val lut %) 1)
        vals  (take (dec (count lut)) iters)]
    (apply str vals)))

(defn output-2 [lut]
  (let [a (next-val lut 1)
        b (next-val lut a)]
    [a b]))

(defn part-1 [n input]
  (let [ds    (read-input input)
        head  (first ds)
        lut   (initialize (pad-input ds 9))
        iters (iterate move-lut [head lut])
        res   (nth iters n)]
    (output-1 (res 1))))

(defn part-2 [n input]
  (let [ds    (read-input input)
        head  (first ds)
        lut   (initialize (pad-input ds 1000000))
        iters (iterate move-lut [head lut])
        res   (nth iters n)]
    (output-2 (res 1))))

(time
 (part-1 100 "389125467"))
;; => "167384529"
(time
 (part-1 100 "974618352"))
;; => "175893264"

(time
 (def part-2-test
   (part-2 10000000 "389125467")))

part-2-test
;; => [934001 159792]

(time
 (apply * part-2-test))
;; => 149245887792

(time
 (def part-2-answer
   (part-2 10000000 "974618352")))

part-2-answer
;; => [422812 90259]

(time
 (apply * part-2-answer))
;; => 38162588308

