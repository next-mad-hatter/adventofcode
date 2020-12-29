(ns aoc-2020.day-11
  (:require [aoc-2020.util :as util]
            [clojure.algo.generic.functor :refer [fmap]]
            [clojure.string :as str]))

(defn lines->seats [lines]
  (let [input (into {} (for [x     (range (count lines))
                             y     (range (count (first lines)))
                             :let  [s (get-in lines [x y])]
                             :when (#{"L" "#"} s)]
                         [[x y] ({"L" false "#" true} s)]))
        seats (keys input)
        occ   (set (map first (filter (fn [[_ v]] true? v) input)))]
    {:seats    seats
     :occupied (if (empty? occ) occ (throw (Exception. "Not implemented")))}))

(defn init [filename]
  (->> filename
       util/fetch-lines
       (mapv #(str/split % #""))
       lines->seats))

(defn change-state-of [thresh hood state coor s]
  (let [cnt (reduce + 0 (map state (hood coor)))]
    (when (or
           (and (= 0 s) (= cnt 0))
           (and (= 1 s) (>= cnt thresh))) [coor (bit-xor 1 s)])))

(defn step [hood thresh state]
  (p ::step
     (let [inds   (keep-indexed (partial change-state-of thresh hood state) state)
           state  (transient state)
           state' (reduce (fn [st [i v]] (assoc! st i v)) state inds)]
       (persistent! state'))))

(defn find-fixed-point [xs]
  (->> xs
       (partition 2 1)
       (filter #(apply = %))
       ffirst))

(defn enumerate [xs]
  (into {} (mapv (comp (partial apply vector) rseq) (map-indexed vector xs))))

(defn solve [construct-hood thresh filename]
  (let [start (init filename)
        hood  (construct-hood (start :seats))
        tr    (enumerate (start :seats))
        hood  (into {} (mapv (fn [[k v]] [(tr k) (filter some? (mapv tr v))]) hood))
        hood  (mapv (comp seq second) (sort-by first (into [] hood)))
        state (vec (repeat (count hood) 0))]
    (->> state
         (iterate (partial step hood thresh))
         (take 100000)
         find-fixed-point
         (reduce + 0))))

;;
;; Part 1
;;

(def eight-dirs
  (for [x     [-1 0 1]
        y     [-1 0 1]
        :when (not= [x y] [0 0])]
    [x y]))

(defn construct-hood-1 [seats]
  (let [mx  (apply max (map first seats))
        my  (apply max (map last seats))
        all (into {} (map (fn [coors] [coors (set (map #(mapv + coors %) eight-dirs))]) seats))]
    (fmap #(set (filter (fn [[x y]] (and (<= 0 x mx) (<= 0 y my))) %)) all)))

(defn part-1 [filename]
  (solve construct-hood-1 4 filename))

(time (part-1 "2020/day_11_small.txt"))
;; => 6

(time (part-1 "2020/day_11_test.txt"))
;; => 37

(time (part-1 "2020/day_11_input.txt"))
;; => 2275

;;
;; Part 2
;;

(defn construct-hood-2 [seats]
  (let [seats (set seats)
        mx    (apply max (map first seats))
        my    (apply max (map last seats))

        move      (fn [dir] (fn [from] (mapv + from dir)))
        ray       (fn [from dir] (rest (iterate (move dir) from)))
        stop-at   (fn [[x y]] (or (not (<= 0 x mx))
                                  (not (<= 0 y my))
                                  (contains? seats [x y])))
        neighbour (fn [coors dir] (util/find-first stop-at (ray coors dir)))

        looks-at (fn [coors] (set (filter some? (map #(->> % (neighbour coors) seats) eight-dirs))))]
    (into {} (map (fn [k] [k (looks-at k)]) seats))))

(defn part-2 [filename]
  (solve construct-hood-2 5 filename))

(time (part-2 "2020/day_11_test.txt"))
;; => 26

(time (part-2 "2020/day_11_input.txt"))
;; => 2121

