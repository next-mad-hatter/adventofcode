(ns aoc-2020.day-23
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn read-input [s]
  (as-> s $
    (str/split $ #"")
    (map edn/read-string $)
    (apply vector-of :int $)))

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
    (into input (apply vector-of :int (range (inc m) (inc len))))))

(defn initialize-lut
  "Creates lookup vector: val -> next-val"
  [head input]
  (let [n (count input)]
    (into
     ;; (vector-of :int head)
     (vector head)
     (map second
          (sort-by first
                   (for [i (range n)]
                     (let [i+ (mod (inc i) n)]
                       (mapv input (vector i i+)))))))))

(defn step [lut]
  (let [lut (transient lut)
        x (lut 0)
        a (lut x)
        b (lut a)
        c (lut b)
        y (lut c)
        l (decrease x (dec (count lut)) #{a b c})
        r (lut l)
        lut'   (assoc! lut x y)
        lut''  (assoc! lut' c r)
        lut''' (assoc! lut'' l a)
        res    (assoc! lut''' 0 y)]
    (persistent! res)))

(defn output-1 [lut]
  (let [iters (iterate #(lut %) 1)
        vals  (take (dec (count lut)) iters)]
    (apply str vals)))

(defn output-2 [lut]
  (let [a (lut 1)
        b (lut a)]
    [a b]))

(defn run [padding formatter n input]
  (let [ds    (read-input input)
        head  (first ds)
        lut   (initialize-lut head (pad-input ds padding))
        iters (iterate step lut)
        res   (nth iters n)]
    (formatter res)))

(def part-1 (partial run 9 output-1))

(def part-2 (partial run 1000000 output-2))

(time
 (run 9 output-1 100 "389125467"))
;; => "167384529"

(time
 (run 9 output-1 100 "974618352"))
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

