(ns aoc-2020.day-20
  (:require [aoc-2020.util :as util]
            [clojure.algo.generic.functor :refer [fmap]]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn read-tile-nr [input]
  (->> input (re-find #"\d+$") edn/read-string))

(defn read-tile-bits [input]
  (as-> input $
      (str/split $ #"\R")
      (mapv (partial mapv {\# 1 \. 0}) $)))

(defn read-tiles [filename]
  (->> filename
       (util/fetch-chunks #"\R\s*\R")
       (map #(str/split % #":\s"))
       (map (juxt #(->> % first read-tile-nr)
                  #(->> % second read-tile-bits)))
       (into {})))

(defn flip [tile]
  (mapv (comp vec reverse) tile))

(defn transpose [tile]
  (apply mapv vector tile))

(def rot
  (comp flip transpose))

(def actions
  "All elements of D_4 as actions on a tile"
  (let [rotations [identity rot (comp rot rot) (comp rot rot rot)]
        flipped (mapv #(comp flip %) rotations)]
    (vec (concat rotations flipped))))

(def sym-count (count actions))

(assert (= 8 sym-count))

;;
;; Part 1
;;

(def boundary-of-tile
  (juxt first #(mapv last %) last #(mapv first %)))

(def tile-configs
  "Given a tile, returns all possible configurations of its boundary,
  each ordered as [N E S W]"
  (let [fns (map #(comp boundary-of-tile %) actions)]
    (apply juxt fns)))

(defn make-boundary-lut
  "Maps tile numbers to possible configurations;
  each tile number maps to 8 vectors of ordered border values"
  [tiles]
  (fmap (fn [x] (fmap (fn [y] (mapv (fn [a] (Long/parseLong (apply str a) 2)) y)) x))
        (fmap tile-configs tiles)))

(defn make-tile-lut
  "Mapping possible border values to owning tile numbers"
  [boundary-lut]
  (let [borders (fmap (comp set flatten) boundary-lut)]
    (apply merge-with set/union
           (mapcat (fn [[k v]] (map hash-map v (repeat #{k}))) borders))))

(defn get-border-in [boundary-lut]
  (fn [[tn cn] dir]
    ;; (get-in boundary-lut [tn cn dir])
    ;; somewhat faster...
    (get (get (get boundary-lut tn {}) cn {}) dir nil)))

(defn some-and-not= [a b]
  (and (some? a) (not= a b)))

(defn aligns
  "Checks if if a tile t oriented as o in cell [x y]
  would align with its neighbours as set in given (partial) solution;
  coordinates use screen convetion, i.e. inverted y-axis"
  [get-border solution [x y] [t o]]
  (cond
    (some-and-not= (get-border (solution [(dec x) y]) 1) (get-border [t o] 3)) false
    (some-and-not= (get-border (solution [x (dec y)]) 2) (get-border [t o] 0)) false
    (some-and-not= (get-border (solution [(inc x) y]) 3) (get-border [t o] 1)) false
    (some-and-not= (get-border (solution [x (inc y)]) 0) (get-border [t o] 2)) false
    :else true))

(defn feasible-for
  "Given a partial solution, enumerates all feasible solutions for grid cell [x y]"
  [sym-count get-border tile-lut free-tiles solution [x y]]
  (let [neighbourders (set (filter some? (map (fn [[x y z]] (get-border (solution [x y]) z))
                                              [[(dec x) y 1] [x (dec y) 2] [(inc x) y 3] [x (inc y) 0]])))
        looked-up (set (mapcat tile-lut neighbourders))
        ;; In our input, all possible borders are only present once or twice,
        ;; hence we want to go through them in order to arrive at a feasible execution time.
        _ (assert (or (empty? solution) (not-empty looked-up)) (str solution " " [x y]))
        constrained (if (empty? looked-up)
                      free-tiles
                      (set/intersection free-tiles looked-up))]
    (for [t constrained
          o (range sym-count)
          :when (aligns get-border solution [x y] [t o])]
      [t o])))

(defn find-solution [sym-count get-border tile-lut free-cells free-tiles solution]
  (if (empty? free-cells)
    solution
    (let [[x y] (first free-cells)
          feasible (feasible-for sym-count get-border tile-lut free-tiles solution [x y])
          free-cells' (rest free-cells)
          find-sub (partial find-solution sym-count get-border tile-lut free-cells')
          subproblems (mapv (fn [[t o]] [(disj free-tiles t) (assoc solution [x y] [t o])]) feasible)
          result (util/find-first some? (map (partial apply find-sub) subproblems))]
      result)))

(defn solve-part-1 [filename]
  (let [tiles (read-tiles filename)
        boundary-lut (make-boundary-lut tiles)
        tile-lut (make-tile-lut boundary-lut)
        grid-width (int (Math/sqrt (count tiles)))
        _ (assert (= (* grid-width grid-width) (count tiles)))
        solution (find-solution sym-count
                                (get-border-in boundary-lut)
                                tile-lut
                                (for [x (range grid-width) y (range grid-width)] [x y])
                                (set (keys tiles))
                                {})
        w (dec grid-width)
        _ (assert (some? solution) "No solution found")
        answer (apply * (mapv (fmap first solution) [[0 0] [0 w] [w 0] [w w]]))
        transformed-tiles (fmap #((actions (second %)) (tiles (first %))) solution)]
    [answer transformed-tiles]))

;;
;; Part 2
;;
(def inner (comp butlast rest))

(defn preprocess [tiles]
  (->> tiles
       (fmap inner)
       (fmap (partial map inner))
       (fmap #(fmap (partial map ["." "#"]) %))))

;; FIXME: deconvolute this
(defn glue [tiles]
  (vec
   (apply concat
          (for [y (sort (into #{} (map first (keys tiles))))]
            (apply map (comp vec concat)
                   (for [x (sort (into #{} (map second (keys tiles))))]
                     (tiles [x y])))))))

(def assemble (comp glue preprocess))

(defn img->str [img]
  (->> img
       (map (partial apply str))
       (str/join "\n")))

(defn all-images [tiles]
  (mapv #(img->str (% (assemble tiles))) actions))

;; FIXME: can we improve the replacement?
(defn m-pattern [n]
  (re-pattern
   (apply str
          "(.{" n "})(..................)#(...*)\n"
          "(.{" n "})#(....)##(....)##(....)###(.*)\n"
          "(.{" n "})(.)#(..)#(..)#(..)#(..)#(..)#(...)(.*)\n")))

(def m-replacement
  (apply str
         "$1$2O$3\n"
         "$4O$5OO$6OO$7OOO$8\n"
         "$9$10O$11O$12O$13O$14O$15O$16$17\n"))

(defn show-monsters [img]
  (let [w (-> img (str/split #"\n") first count)
        tr (fn [offset] #(str/replace % (m-pattern offset) m-replacement))
        trs (map tr (range (- w 19)))]
    (reduce (fn [i f] (f i)) img trs)))

(defn count-meshes [s]
  (->> s seq (filter #{\#}) count))

(defn solve-part-2 [tiles]
  (->> tiles
       all-images
       (map show-monsters)
       (map count-meshes)
       (apply min)))

;;
;; Results
;;

(time
 (def part-1-solutions (mapv solve-part-1 ["2020/day_20_small.txt" "2020/day_20_test.txt" "2020/day_20_input.txt"])))

(map first part-1-solutions)
;; => (110355024 20899048083289 15003787688423)

(def all-tiles
  (mapv second part-1-solutions))

(time
 (mapv solve-part-2 all-tiles))
;; => [8 273 1705]

(def test-tiles (all-tiles 1))

(.indexOf (all-images test-tiles) ".#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###")

(some? (some #{".#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###"} (all-images test-tiles)))
