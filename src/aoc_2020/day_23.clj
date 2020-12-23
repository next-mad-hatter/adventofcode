(ns aoc-2020.day-23
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            #_[clojure.core.rrb-vector :as fv]))

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
    (into
     input
     (apply vector-of :int (range (inc m) (inc len))))))

(defn initialize-lut
  "Creates lookup vector: val -> next-val"
  [input]
  (let [n (count input)]
    (into
     ;; we don't want to deal with 0-based lookup right now
     (vector-of :int 42)
     ;; Note: rrb vector might be broken with jdk 11 and clojure 1.10.1
     ;; (fv/vector-of :int 42)
     (map second
          (sort-by first
                   (for [i (range n)]
                     (let [i+ (mod (inc i) n)]
                       (mapv input (vector i i+)))))))))

(defn move-lut [[head lut]]
  (let [x head
        a (lut x)
        b (lut a)
        c (lut b)
        y (lut c)
        ;; NB: this assumes one-padded vector lookup
        l (decrease x (dec (count lut)) #{a b c})
        r (lut l)
        ops [#(assoc % x y)
             #(assoc % c r)
             #(assoc % l a)]
        ;; Note: with rrb vector and padding >= 32, the application fails here,
        ;;       but it's unclear why/how exactly and seems weirdly hard to reproduce
        lut' ((apply comp ops) lut)]
    [(lut' head) lut']))

(defn output-1 [lut]
  (let [iters (iterate #(lut %) 1)
        ;; NB: this assumes one-padded vector lookup
        vals  (take (dec (count lut)) iters)]
    (apply str vals)))

(defn output-2 [lut]
  (let [a (lut 1)
        b (lut a)]
    [a b]))

(defn run [padding formatter n input]
  (let [ds    (read-input input)
        head  (first ds)
        lut   (initialize-lut (pad-input ds padding))
        iters (iterate move-lut [head lut])
        res   (nth iters n)]
    (formatter (res 1))))

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

