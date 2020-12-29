(ns aoc-2020.day-25
  (:require [clojure.set :as set]))

(set! *unchecked-math* true)

(defn mod-pow [b e m]
  (int
   (.modPow (BigInteger/valueOf b)
            (BigInteger/valueOf e)
            (BigInteger/valueOf m))))

(defn step-log
  [base p]
  (let [mod-p #(mod % p)
        m     (int (Math/ceil (Math/sqrt p)))
        pows  (take m (iterate #(mod-p (* base %)) 1))
        lut   (set/map-invert (into {} (map-indexed vector pows)))
        inv   (mod-pow base (- m) p)]
    (fn [n]
      (loop [n n
             i 0]
        (if (lut n)
          (mod-p (+ (lut n) (* m i)))
          (recur (mod-p (* n inv)) (inc i)))))))

(def p 20201227)

(time
 (def log (step-log 7 p)))

(defn decrypt [a b]
  (mod-pow a (log b) p))

(time
 (decrypt 5764801 17807724))
;; => 14897079

(time
 (decrypt 8184785 5293040))
;; => 4126980

