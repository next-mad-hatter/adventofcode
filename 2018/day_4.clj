(ns madhat.adventofcode.day-three
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.algo.generic.functor :as gf :only (fmap)]))


;; Day 4


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
  "While the guards might start their shift before or after midnight,
  we want to set the shift start to 00:00 exactly."
  (if (.isBefore t (.withHour t 12))
    (-> t (.withHour 0) (.withMinute 0))
    (-> t (.withHour 0) (.withMinute 0) (.plusDays 1))))


(defn decode-record
  [x]
  (let [[date does-what val] x
        day                  (-> date shift-start str)
        minute               (.getMinute date)
        guard-id             (when (not (nil? val)) (read-string val))
        can-do               {"wakes up" :awake "falls asleep" :asleep}
        status               (if (nil? guard-id) (can-do does-what) :awake)
        adj-minute           (if (nil? guard-id) minute 0)]
    (into {} (filter (comp not nil? second) {:day day :minute adj-minute :status status :guard-id guard-id}))))


(defn parse-input [input]
  (->> input
       (re-seq #"\s+\[(\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2})\]\s+(Guard\s+#(\d+)\s+begins|falls\s+asleep|wakes\s+up).*\n")
       (pmap rest)
       (pmap vec)
       (pmap #(update % 0 to-date))
       (pmap decode-record)))


(defn propagate-guard-id
  "After the input had been parsed, only the 'guard begins shift' entry
  will have a guard id.  If we group them by day, we can set guard id for
  all records of a day using this."
  [timeline]
  (let [g-id (:guard-id (first timeline))]
    (pmap #(assoc % :guard-id g-id) timeline)))


(defn spell-out-timeline [p q]
  "We will fill the timeline by creating an entry for each minute.
  This computes such elements for a pair of (consequtive) timeline entries."
  (let [x  (:minute p)
        y  (:minute q)
        r  (range x y)
        ts (map #(assoc p :minute %) r)]
    ts))


(defn fill-timeline [t]
  "Fill the timeline by creating an entry for each minute."
  (let [xs t
        ys (concat (rest t) [(assoc (first t) :minute 61)])]
    (mapcat spell-out-timeline xs ys)))


(defn create-timelines [input]
  "Now we can convert our input to a map assigning to each guard (id)
  a sequence of timelines, one for each day.  Each timeline is a sequence
  of status changes we discovered in our input.
  "
  (->> input
       (parse-input)
       (group-by :day)
       (gf/fmap propagate-guard-id)
       (vals)
       (reduce into)
       (group-by :guard-id)
       (gf/fmap #(group-by :day %))
       (gf/fmap vals)
       (gf/fmap (fn [days] (pmap #(sort-by :minute %) days)))
       (gf/fmap #(map fill-timeline %))))


(def timelines (create-timelines input))


;; Part 1

;; We don't care about days here

(def max-g-id
  (->> timelines
      (gf/fmap #(reduce into %))
      (gf/fmap #(group-by :status %))
      (gf/fmap #(gf/fmap count %))
      (apply max-key #(:asleep (val %)))
      (key)))

(def max-minute
  (let [ts (get timelines max-g-id)]
    (->> ts
        (reduce into)
        (filter #(= (:status %) :asleep))
        (group-by :minute)
        (gf/fmap count)
        (apply max-key val)
        (key))))

(* max-g-id max-minute)

;; DEBUG

(def spy #(do (println "DEBUG:" %) %))

(pp/pp)

;; TESTS

(def d (create-timelines input))

(def t ((comp first second first) d))

d

t

;; ;; NullPointerException ?! :D
;; ((->> sort-by :minute) [{:minute 3} {:minute 0} {:minute 5}]) 

