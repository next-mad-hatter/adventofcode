(ns aoc-2020.day-25)

(def P 20201227)

(defn zp [n]
  (mod n P))

(defn ×7 [n]
  (zp (* 7 n)))

(defn enc [n]
  (loop [n (mod n (dec P))
         r 1]
    (if (= n 0) r (recur (dec n) (×7 r)))))

(defn log [x]
  (let [x (zp x)]
    (loop [n 7
           r 1]
      (if (= x n) r (recur (×7 n) (inc r))))))

(defn enc-key [a b]
  (enc (* (log a) (log b))))

(enc-key
 5764801
 17807724)
;; => 14897079

(enc-key
  8184785
  5293040)
;; => 4126980
