(ns aoc-2020.day-11
  (:require [aoc-2020.util :as util]
            [clojure.algo.generic.functor :as gf]
            [clojure.string :as str]
            [taoensso.tufte :as tufte :refer [p profile]]))

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
  (let [cnt (count (filter true? (map state (hood coor))))]
    (when (or
           (and (not s) (= cnt 0))
           (and s (>= cnt thresh))) coor)))

(defn step [hood thresh state]
  (let [inds   (keep-indexed (partial change-state-of thresh hood state) state)
        state' (reduce (fn [st i] (assoc st i (not (st i)))) state inds)]
    state'))

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
        ;; TODO: try this
        ;; state (into (vector-of :int) (repeat (count hood) false))
        state (vec (repeat (count hood) false))
        ]
    (->> state
         (iterate (partial step hood thresh))
         (take 10000)
         find-fixed-point
         (filter true?)
         count)))

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
    (gf/fmap #(set (filter (fn [[x y]] (and (<= 0 x mx) (<= 0 y my))) %)) all)))

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

;;
;; Until we're done tinkering, we'll leave this here
;;

(tufte/add-basic-println-handler! {})

(comment)
(time
 (profile
  {}
  (let [start  (init "2020/day_11_input.txt")
        hood   (construct-hood-2 (start :seats))
        thresh 5
        tr     (enumerate (start :seats))
        hood   (p ::h0 (into {} (mapv (fn [[k v]] [(tr k) (filter some? (mapv tr v))]) hood)))
        hood   (p ::h1 (mapv (comp seq second) (sort-by first (into [] hood))))
        state  (vec (repeat (count hood) false))]
    (->> state
         (iterate (partial step hood thresh))
         (take 100)
         (doall)
         last
         (filter true?)
         count))))
