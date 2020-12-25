(ns aoc-2020.day-25
  (:require [clojure.set :as set]))

(def P 20201227)

(defn mod-p [n] (mod n P))

(defn ×7 [n]
  (mod-p (* 7 n)))

(assert (> Long/MAX_VALUE (* P P)))

(defn enc
  "Computes nth power of 7 (mod P)"
  [n]
  (loop [r 1
         b 7
         n (mod n (dec P))]
    (cond
      (= n 0)   r
      (= n 1)   (mod-p (* b r))
      (even? n) (recur r (mod-p (* b b)) (/ n 2))
      :else     (recur (mod-p (* b r)) (mod-p (* b b)) (/ (dec n) 2)))))

(defn xgcd
  "Extended Euclidean algorithm (mod P)"
  [a b]
  (loop [[r' r] [a b]
         [s' s] [1 0]
         [t' t] [0 1]]
    (if (= r 0)
      [r' s' t']
      (let [q      (int (/ r' r))
            [r' r] [r (mod-p (- r' (* r q)))]
            [s' s] [s (mod-p (- s' (* s q)))]
            [t' t] [t (mod-p (- t' (* t q)))]]
        (recur [r' r] [s' s] [t' t])))))

(defn step-log
  "Discrete logarithm base 7 mod P"
  []
  (let [m    (int (Math/ceil (Math/sqrt P)))
        pows (take m (iterate ×7 1))
        lut  (set/map-invert (into {} (map-indexed vector pows)))
        inv  (second (xgcd (enc m) P))]
    (fn [b]
      (loop [b b
             i 0]
        (if (lut b)
          (mod-p (+ (lut b) (* i m)))
          (recur (mod-p (* b inv)) (inc i)))))))

(def log (step-log))

(defn enc-key [a b]
  (enc (* (log a) (log b))))

(time
 (enc-key
  5764801
  17807724))
;; => 14897079

(time
 (enc-key
  8184785
  5293040))
;; => 4126980
