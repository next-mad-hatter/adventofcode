(ns aoc-2020.day-11
  (:require [aoc-2020.util :as util]
            [clojure.algo.generic.functor :as gf]
            [clojure.string :as str]
            [taoensso.tufte :as tufte :refer [p profile]]))

(defn lines->seats [lines]
  (let [input (into {} (for [x (range (count lines))
                             y (range (count (first lines)))
                             :let [s (get-in lines [x y])]
                             :when (#{"L" "#"} s)]
                         [[x y] ({"L" false "#" true} s)]))
        seats (keys input)
        occ (set (map first (filter (fn [[_ v]] true? v) input)))]
    {:seats seats
     :occupied (if (empty? occ) occ (throw (Exception. "Not implemented")))}))

(defn init [filename]
  (->> filename
       util/fetch-lines
       (mapv #(str/split % #""))
       lines->seats))

(defn new-state-of [thresh coor cnt s]
  (cond
    (and (not s) (= cnt 0)) [coor true]
    (and s (>= cnt thresh)) [coor false]
    :else nil))

(defn step [hood thresh state]
  (let [occupied      (state :occupied)
        occ-neigh-cnt (state :occ-neigh-cnt)
        flips         (p ::flips (vec (filter some? (map (partial new-state-of thresh) (range) occ-neigh-cnt occupied))))
        next-occ      (p ::next-occ (reduce (fn [o [k v]] (assoc o k v)) occupied flips))
        cnt-groups    (p ::cnt-groups (group-by second (filter (comp some? hood first) flips)))
        cnt-items     (p ::cnt-items (gf/fmap #(mapcat (comp hood first) %) cnt-groups))
        next-cnt      (p ::next-cnt+ (reduce (fn [c k] (update c k inc)) occ-neigh-cnt (cnt-items true {})))
        next-cnt      (p ::next-cnt- (reduce (fn [c k] (update c k dec)) next-cnt (cnt-items false {})))]
    {:occupied next-occ, :occ-neigh-cnt next-cnt}))

(defn find-fixed-point [xs]
  (->> xs
       (partition 2 1)
       (filter #(apply = %))
       ffirst))

(defn enumerate [xs]
  (into {} (mapv (comp (partial apply vector) rseq) (map-indexed vector xs))))

(defn solve [construct-hood thresh filename]
  (let [start (init filename)
        hood (construct-hood (start :seats))
        tr (enumerate (start :seats))
        hood (into {} (mapv (fn [[k v]] [(tr k) (filter some? (mapv tr v))]) hood))
        hood (mapv second (sort-by first (into [] hood)))
        occupied (vec (repeat (count hood) false))
        occ-neigh-cnt (vec (repeat (count hood) 0))]
    (->> {:occupied occupied, :occ-neigh-cnt occ-neigh-cnt}
         (iterate (partial step hood thresh))
         (take 100000)
         (map :occupied)
         find-fixed-point
         (filter true?)
         count)))

;;
;; Part 1
;;

(def eight-dirs
  (for [x [-1 0 1]
        y [-1 0 1]
        :when (not= [x y] [0 0])]
    [x y]))

(defn construct-hood-1 [seats]
  (let [mx (apply max (map first seats))
        my (apply max (map last seats))
        all (into {} (map (fn [coors] [coors (set (map #(mapv + coors %) eight-dirs))]) seats))]
    (gf/fmap #(set (filter (fn [[x y]] (and (<= 0 x mx) (<= 0 y my))) %)) all)))

(defn part-1 [filename]
  (solve construct-hood-1 4 filename))

(comment
  (time (part-1 "2020/day_11_small.txt")))
;; => 6

(time (part-1 "2020/day_11_test.txt"))
;; => 37

;; Takes appr. 1.5 sec w/ i5-7200U or ? secs w/ i5-8265U
(comment
  (time (part-1 "2020/day_11_input.txt")))
;; => 2275

;;
;; Part 2
;;

(defn construct-hood-2 [seats]
  (let [seats     (set seats)
        mx        (apply max (map first seats))
        my        (apply max (map last seats))

        move      (fn [dir] (fn [from] (mapv + from dir)))
        ray       (fn [from dir] (rest (iterate (move dir) from)))
        stop-at   (fn [[x y]] (or (not (<= 0 x mx))
                                  (not (<= 0 y my))
                                  (contains? seats [x y])))
        neighbour (fn [coors dir] (util/find-first stop-at (ray coors dir)))

        looks-at  (fn [coors] (set (filter some? (map #(->> % (neighbour coors) seats) eight-dirs))))]
    (into {} (map (fn [k] [k (looks-at k)]) seats))))

(defn part-2 [filename]
  (solve construct-hood-2 5 filename))

(time (part-2 "2020/day_11_test.txt"))
;; => 26

;; Takes appr. 1.5 secs w/ i5-7200U or 1.3 secs w/ i5-8265U
(comment
  (time (part-2 "2020/day_11_input.txt")))
;; => 2121

;;
;; Until we're done tinkering, we'll leave this here
;;

(tufte/add-basic-println-handler! {})

(comment
  (time
   (profile
    {}
    (ffirst
     (mapv identity
           (let [start (init "2020/day_11_input.txt")
                 hood (construct-hood-2 (start :seats))
                 thresh 5
                 tr (enumerate (start :seats))
                 hood (into {} (mapv (fn [[k v]] [(tr k) (filter some? (mapv tr v))]) hood))
                 hood (mapv second (sort-by first (into [] hood)))
                 occupied (vec (repeat (count hood) false))
                 occ-neigh-cnt (vec (repeat (count hood) 0))]
             (->> {:occupied occupied, :occ-neigh-cnt occ-neigh-cnt}
                  (iterate (partial step hood thresh))
                  (take 30)
                  (doall)
                  first)))))))
