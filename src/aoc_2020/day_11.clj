(ns aoc-2020.day-11
  (:require [aoc-2020.util :as util]
            [clojure.set :as set]
            [clojure.string :as str]
            [taoensso.tufte :as tufte :refer [p profile]]))

;; TODO: How do we make our state update faster?

(defn lines->seats [lines]
  (for [x (range (count lines))
        y (range (count (first lines)))
        :let [s (get-in lines [x y])]
        :when (#{"L" "#"} s)]
    [[x y] ({"L" false "#" true} s)]))

(defn init [filename]
  (->> filename
       util/fetch-lines
       (mapv #(str/split % #""))
       lines->seats
       (into {})))

(defn next-state-of [hood thresh occupied coors]
  (let [cnt (count (p ::filter (set/intersection occupied (p ::hood (hood coors)))))
        s   (contains? occupied coors)]
    (cond
      (and (not s) (= cnt 0)) true
      (and s (>= cnt thresh)) false
      :else s)))

(defn step [hood thresh state]
  (let [occupied (set (keys (filter (fn [[_ v]] (true? v)) state)))]
    (into {} (map #(do [% (next-state-of hood thresh occupied %)]) (keys state)))))

(defn find-fixpoint [xs]
  (->> xs
       (partition 2 1)
       (filter #(apply = %))
       ffirst))

(defn occupied-total [state]
  (count (filter (fn [[_ v]] true? v) state)))

(defn solve [construct-hood thresh filename]
  (let [start (init filename)
        hood  (construct-hood start)]
    (->> start
         (iterate (partial step hood thresh))
         (take 100000)
         find-fixpoint
         occupied-total)))

;;
;; Part 1
;;

(defn construct-hood-1 [state]
  (let [dirs (for [x [-1 0 1]
                   y [-1 0 1]
                   :when (not= [x y] [0 0])] [x y])]
    (into {}
          (map
           (fn [coors]
             [coors (set (map #(mapv + coors %) dirs))])
           (keys state)))))

(time (solve construct-hood-1 4 "2020/day_11_test.txt"))
;; => 37

;; takes appr 3-4secs atm
;; (time (solve construct-hood-1 4 "2020/day_11_input.txt"))
;; => 2275

;;
;; Part 2
;;

(defn construct-hood-2 [state]
  (let [seats     (set (keys state))
        mx        (apply max (map first seats))
        my        (apply max (map last seats))

        dirs      (for [x [-1 0 1]
                        y [-1 0 1]
                        :when (not= [x y] [0 0])]
                    [x y])

        move      (fn [dir] (fn [from] (mapv + from dir)))
        ray       (fn [from dir] (rest (iterate (move dir) from)))
        stop-at   (fn [[x y]] (or (not (<= 0 x mx))
                                  (not (<= 0 y my))
                                  (contains? state [x y])))
        neighbour (fn [coors dir] (util/find-first stop-at (ray coors dir)))

        looks-at  (fn [coors] (set (filter some? (map #(->> % (neighbour coors) seats) dirs))))]
    (into {} (map (fn [[k _]] [k (looks-at k)]) state))))

(time (solve construct-hood-2 5 "2020/day_11_test.txt"))
;; => 26

;; Takes 2-3secs
;; (time (solve construct-hood-2 5 "2020/day_11_input.txt"))
;; => 2121


;;
;; Some profiling...
;;

(tufte/add-basic-println-handler! {})

(def start (init "2020/day_11_input.txt"))

(def hood (construct-hood-1 start))

hood

(time
 (profile
  {}
  (ffirst
   (mapv identity
         (take 20 (iterate (partial step hood 4) start))))))
