(ns madhat.adventofcode.day-three
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.algo.generic.functor :as gf :only (fmap)]))


;; Day 3


(def input "
  [1518-11-01 00:00] Guard #10 begins shift
  [1518-11-01 00:05] falls asleep
  [1518-11-01 00:25] wakes up
  [1518-11-01 00:30] falls asleep
  [1518-11-01 00:55] wakes up
  [1518-11-01 23:58] Guard #99 begins shift
  [1518-11-02 00:40] falls asleep
  [1518-11-02 00:50] wakes up
  [1518-11-03 00:05] Guard #10 begins shift
  [1518-11-03 00:24] falls asleep
  [1518-11-03 00:29] wakes up
  [1518-11-04 00:02] Guard #99 begins shift
  [1518-11-04 00:36] falls asleep
  [1518-11-04 00:46] wakes up
  [1518-11-05 00:03] Guard #99 begins shift
  [1518-11-05 00:45] falls asleep
  [1518-11-05 00:55] wakes up
")


(defn to-date [s]
  (java.time.LocalDateTime/parse s (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))) 


(defn shift-start [t]
  (if (.isBefore t (.withHour t 12))
    (-> t (.withHour 0) (.withMinute 0))
    (-> t (.withHour 0) (.withMinute 0) (.plusDays 1))))


(defn decode-record [x]
  (let [[date does-what val] x
        day                  (-> date shift-start str)
        minute               (.getMinute date)
        guard-id             (when (not (nil? val)) (read-string val))
        can-do               {"wakes up" :awake "falls asleep" :asleep}
        status               (if (nil? guard-id) (can-do does-what) :awake)]
    (into {} (filter (comp not nil? second) {:day day :minute minute :status status :guard-id guard-id}))))


(defn parse-input [input]
  (->> input
       (re-seq #"\s+\[(\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2})\]\s+(Guard\s+#(\d+)\s+begins|falls\s+asleep|wakes\s+up).*\n")
       (pmap rest)
       (pmap vec)
       (pmap #(update % 0 to-date))
       ;; (sort-by first)
       (pmap decode-record)))


(defn set-guard-id [timeline]
  (let [g-id (:guard-id (first timeline))]
    (map #(assoc % :guard-id g-id) timeline)))


;; TESTS

(def i (parse-input input))
(def r (first i))
(def g (group-by :day i))
(def s (gf/fmap set-guard-id g))

(pp/pp)

