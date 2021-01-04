(ns common.hexanimate
  (:require [aoc-2020.day-24 :as sim]
            [clojure.set :as set]
            [quil.core :as q :include-macros true]
            [quil.middleware :as qm]))

(def image-width 1200)
(def image-height 720)
(def fg-color [88 110 117])
(def bg-color [253 246 227])
(def save-images false)

;; Afterwards we can use e.g.
;;
;; $  ffmpeg -framerate 10 -i /tmp/sim-%05d.png -s:v 1200x720 -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p /tmp/sim-output.mp4
;;

(defn simulate [filename n]
  (->> filename
       sim/input->blacks
       (iterate sim/step)
       (take n)))

;; (def sim-data (simulate "2020/day_24_test.txt" 100))
(def sim-data (simulate "2020/day_24_input.txt" 100))

(def test-data
  [#{[-3 0] [3 5] [-10 6] [1 2] [2 2]}
   #{[-2 0] [-10 6] [2 2]}
   #{[-3 0] [3 5] [1 2] [2 2]}])

(def sq3 (Math/sqrt 3))

(defn tr-data [data]
  (let [all (mapcat identity data)
        mq  (apply min (map first all))
        mr  (apply min (map second all))
        cs  (map (fn [s] (map #(mapv - % [(dec mq) (dec mr)]) s)) data)]
    {:blacks (map set cs)
     :max-x  (- (apply max (map first all)) (dec mq))
     :max-y  (- (apply max (map second all)) (dec mr))}))

;; (def data
;;   (tr-data test-data))

(def data
  (tr-data sim-data))

(def tile-size
  (min
   (int (Math/floor (/ image-width (+ (* 0.5 (:max-y data)) (* sq3 (:max-x data))))))
   (int (Math/floor (/ image-height 1.5 (:max-y data))))))

(defn hex->pixel [size [q r]]
  [(* size sq3 (+ q (* r 0.5)))
   (* size r 1.5)])

(defn hex-outline [size [x y]]
  (let [w  (/ (* size sq3) 2)
        h  (/ size 2)
        cs [[ 0 -2]
            [ 1 -1]
            [ 1  1]
            [ 0  2]
            [-1  1]
            [-1 -1]]]
    (->> cs (map (fn [[a b]] [(+ x (* a w)) (+ y (* b h))])))))

(defn hex-shapes [s [x y]]
  (let [[a b c d e f] (hex-outline s [x y])]
    {:triangles
     [[a b c] [d e f] [a c d] [a d f]]
     :lines
     [[a b] [b c] [c d] [d e] [e f] [f a]]}))

(defn draw-full-hex [size i j]
  (let [triangles (:triangles (hex-shapes size (hex->pixel size [i j])))]
    (q/begin-shape :triangles)
    (doseq [s triangles]
      (doseq [p s] (apply q/vertex p)))
    (q/end-shape)))

(defn draw-empty-hex [size i j]
  (let [lines (:lines (hex-shapes size (hex->pixel size [i j])))]
    (q/begin-shape :lines)
    (doseq [s lines]
      (doseq [p s] (apply q/vertex p)))
    (q/end-shape)))

(defn draw-filled-hex [size i j]
  (let [triangles (:triangles (hex-shapes size (hex->pixel size [i j])))]
    (q/begin-shape :triangles)
    (doseq [t triangles]
      (doseq [p t] (apply q/vertex p)))
    (q/end-shape)))

(defn setup []
  (q/frame-rate 100)
  (q/stroke-weight 1)
  ;; (q/background (apply q/color bg-color))
  (q/fill (apply q/color bg-color))
  (q/rect 0 0 image-width image-height)
  ;; (q/stroke (apply q/color fg-color))
  ;; (doseq [q (range 1 (inc (:max-x data)))
  ;;         r (range 1 (inc (:max-y data)))]
  ;;   (draw-empty-hex tile-size q r))
  {:frame   0
   :current (-> data :blacks first)
   ;;:fill    (-> data :blacks first)
   ;;:clear   '()
   :rest    (-> data :blacks rest)})

(defn update-state [state]
  (if (empty? (:rest state))
    (do
      (q/no-loop)
      state)
    (let [old-blacks (-> state :current)
          new-blacks (-> state :rest first)]
      {:frame   (-> state :frame inc)
       :current new-blacks
       ;;:fill    (set/difference new-blacks old-blacks)
       ;;:clear   (set/difference old-blacks new-blacks)
       :rest    (-> state :rest rest)})))

(defn draw-state [state]
  (q/fill (apply q/color bg-color))
  (q/rect 0 0 image-width image-height)

  (q/stroke (apply q/color fg-color))
  (q/fill (apply q/color fg-color))
  (doseq [[q r] (:current state)]
    (draw-filled-hex tile-size q r))
  ;; (doseq [[q r] (:fill state)]
  ;;   (draw-filled-hex tile-size q r))

  ;; (q/stroke (apply q/color bg-color))
  ;; (q/fill (apply q/color bg-color))
  ;; (doseq [[q r] (:clear state)]
  ;;   (draw-filled-hex tile-size q r))
  (when save-images (q/save (format "/tmp/sim-%05d.png" (:frame state)))))

(q/defsketch hexperiment
  :title "Hexperiment"
  :settings #(q/smooth 2)
  :middleware [qm/fun-mode]
  :setup setup
  :draw draw-state
  :update update-state
  :size [image-width image-height])
