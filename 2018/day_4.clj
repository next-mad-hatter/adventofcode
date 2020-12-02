(ns madhat.adventofcode.day-three
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.algo.generic.functor :as gf :only (fmap)]))


;; Day 4


(defn to-date [s]
  (java.time.LocalDateTime/parse s (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))) 


(defn shift-date [t]
  "While the guards might start their shift before or after midnight,
  we want to set the shift start to 00:00 exactly."
  (if (.isBefore t (.withHour t 12))
    (-> t (.withHour 0) (.withMinute 0))
    (-> t (.withHour 0) (.withMinute 0) (.plusDays 1))))


(defn decode-record
  [x]
  (let [[date does-what val] x
        day                  (-> date shift-date)
        minute               (.getMinute date)
        guard-id             (when (not (nil? val)) (read-string val))
        can-do               {"wakes up" :awake "falls asleep" :asleep}
        status               (if (nil? guard-id) (can-do does-what) :awake)
        ;; adj-minute           (if (nil? guard-id) minute 0)  ;; if we only count minutes asleep, we don't care about late shift start;  if we do -- do we count the minutes before as awake?..
        adj-minute           (if (.isBefore date day) (- minute 60) minute)]
    (if (empty? (filter (comp not nil?) [guard-id status])) (throw (Exception. (str x))))
    (into {} (filter (comp not nil? second) {:day (str day) :minute adj-minute :status status :guard-id guard-id}))))


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
  all records of a day using this.  We expect a sorted timeline here."
  [timeline]
  (let [g-id (:guard-id (first timeline))]
    (if (nil? g-id) (throw (Exception. (str timeline))))
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
       (gf/fmap #(sort-by :minute %))
       (gf/fmap propagate-guard-id)
       (vals)
       (reduce into)
       (group-by :guard-id)
       (gf/fmap #(group-by :day %))
       (gf/fmap vals)
       (gf/fmap (fn [days] (pmap #(sort-by :minute %) days)))
       (gf/fmap #(map fill-timeline %))
       (gf/fmap (fn [days] (pmap (fn [ts] (filter #(>= (% :minute) 0) ts)) days)))))


(def timelines (create-timelines input))


;; Part 1
;; We don't care about days here

(def max-g-id
  (->> timelines
      (gf/fmap #(reduce into %))
      (gf/fmap #(group-by :status %))
      (gf/fmap #(merge {:asleep 0 :awake 0} (gf/fmap count %)))
      (apply max-key #(:asleep (val %)))
      (key)))

max-g-id


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


;; Part 2
;; We don't care about days here.
;; Now we also know that we don't care about minutes
;; we're awake at all either.


(def sol
  (->> timelines
       (gf/fmap #(reduce into %))
       (gf/fmap (fn [d] (filter #(= (:status %) :asleep) d)))
       (gf/fmap #(map :minute %))
       (gf/fmap frequencies)
       (filter (comp not-empty val))
       (into {})
       (gf/fmap #(apply max-key val %))
       (apply max-key (comp second val))))

(* (key sol) (first (val sol)))


;; DEBUG

(def input "
  [1999-01-01 23:20] Guard #1 begins shift
  [1999-01-01 23:21] falls asleep
  [1999-01-02 00:00] wakes up
  [1518-11-03 00:29] wakes up
  [1518-11-01 00:05] falls asleep
  [1518-11-01 00:25] wakes up
  [1518-11-05 00:55] wakes up
  [1518-11-01 00:00] Guard #10 begins shift
  [1518-11-01 00:30] falls asleep
  [1518-11-01 00:55] wakes up
  [1518-11-01 23:58] Guard #99 begins shift
  [1518-11-02 00:40] falls asleep
  [1518-11-02 00:50] wakes up
  [1518-11-03 00:05] Guard #10 begins shift
  [1518-11-03 00:24] falls asleep
  [1518-11-04 00:02] Guard #99 begins shift
  [1518-11-04 00:36] falls asleep
  [1518-11-04 00:46] wakes up
  [1518-11-05 00:03] Guard #99 begins shift
  [1518-11-05 00:45] falls asleep
  ")


(def spy #(do (println "DEBUG:" %) %))

(pp/pp)

;; TESTS

(def d (create-timelines input))

(def t ((comp first second first) d))

d

t

;; ;; NullPointerException ?! :D
;; ((->> sort-by :minute) [{:minute 3} {:minute 0} {:minute 5}]) 


;; INPUT

(def input "
  [1518-03-18 00:03] Guard #3529 begins shift
  [1518-04-26 00:55] wakes up
  [1518-04-30 00:46] wakes up
  [1518-10-28 00:01] Guard #2011 begins shift
  [1518-03-27 00:58] wakes up
  [1518-05-14 00:36] wakes up
  [1518-04-08 00:18] falls asleep
  [1518-09-20 23:59] Guard #2011 begins shift
  [1518-07-30 00:27] wakes up
  [1518-04-22 00:56] falls asleep
  [1518-06-25 00:00] Guard #509 begins shift
  [1518-07-04 00:22] falls asleep
  [1518-08-05 00:39] falls asleep
  [1518-04-13 00:09] falls asleep
  [1518-06-13 00:56] wakes up
  [1518-07-02 00:40] falls asleep
  [1518-09-02 00:07] falls asleep
  [1518-07-23 23:59] Guard #2179 begins shift
  [1518-10-03 00:42] falls asleep
  [1518-11-17 00:32] wakes up
  [1518-08-05 00:57] wakes up
  [1518-05-02 00:03] Guard #509 begins shift
  [1518-04-06 00:54] wakes up
  [1518-11-15 00:03] Guard #1973 begins shift
  [1518-11-23 00:00] Guard #1877 begins shift
  [1518-11-21 00:38] wakes up
  [1518-11-01 23:59] Guard #2693 begins shift
  [1518-05-03 00:30] falls asleep
  [1518-06-16 00:49] wakes up
  [1518-08-13 23:46] Guard #317 begins shift
  [1518-11-13 00:19] falls asleep
  [1518-08-20 00:02] Guard #1949 begins shift
  [1518-04-15 00:46] falls asleep
  [1518-09-01 00:00] Guard #2179 begins shift
  [1518-03-19 00:51] wakes up
  [1518-07-19 00:53] wakes up
  [1518-08-25 00:40] wakes up
  [1518-08-13 00:01] Guard #1973 begins shift
  [1518-09-11 00:02] Guard #3529 begins shift
  [1518-10-14 00:04] falls asleep
  [1518-08-28 00:29] wakes up
  [1518-10-17 00:12] falls asleep
  [1518-06-05 00:05] falls asleep
  [1518-07-18 00:59] wakes up
  [1518-11-08 00:02] Guard #853 begins shift
  [1518-11-04 00:47] wakes up
  [1518-06-01 00:52] wakes up
  [1518-10-13 23:48] Guard #3529 begins shift
  [1518-04-12 00:05] falls asleep
  [1518-08-07 00:00] Guard #2137 begins shift
  [1518-03-31 00:46] wakes up
  [1518-05-28 00:54] falls asleep
  [1518-06-14 00:43] wakes up
  [1518-11-11 23:59] Guard #317 begins shift
  [1518-08-24 00:57] wakes up
  [1518-08-05 23:47] Guard #3373 begins shift
  [1518-07-12 00:57] wakes up
  [1518-10-16 00:56] wakes up
  [1518-08-26 00:36] wakes up
  [1518-05-18 00:59] wakes up
  [1518-07-02 00:20] falls asleep
  [1518-08-05 00:49] wakes up
  [1518-05-24 23:58] Guard #641 begins shift
  [1518-06-01 00:44] wakes up
  [1518-04-30 00:32] falls asleep
  [1518-09-05 00:44] wakes up
  [1518-09-12 00:31] falls asleep
  [1518-11-21 23:59] Guard #853 begins shift
  [1518-04-07 00:06] falls asleep
  [1518-03-17 00:28] falls asleep
  [1518-06-10 00:10] falls asleep
  [1518-05-19 00:58] wakes up
  [1518-06-03 00:27] falls asleep
  [1518-04-13 23:56] Guard #317 begins shift
  [1518-06-27 23:52] Guard #853 begins shift
  [1518-03-25 00:00] Guard #641 begins shift
  [1518-05-07 00:54] falls asleep
  [1518-09-22 00:58] wakes up
  [1518-03-28 00:47] falls asleep
  [1518-11-13 23:48] Guard #401 begins shift
  [1518-10-07 00:52] wakes up
  [1518-07-24 23:58] Guard #1949 begins shift
  [1518-04-16 00:03] Guard #509 begins shift
  [1518-09-07 00:44] wakes up
  [1518-04-18 00:50] falls asleep
  [1518-06-19 00:00] Guard #641 begins shift
  [1518-03-29 00:37] falls asleep
  [1518-11-18 23:59] Guard #1049 begins shift
  [1518-11-14 00:02] falls asleep
  [1518-06-24 00:03] Guard #2029 begins shift
  [1518-03-27 00:42] wakes up
  [1518-11-20 00:23] falls asleep
  [1518-05-03 00:50] wakes up
  [1518-03-28 00:19] falls asleep
  [1518-09-07 23:59] Guard #2693 begins shift
  [1518-11-06 23:59] Guard #3529 begins shift
  [1518-03-16 00:34] falls asleep
  [1518-08-09 00:10] falls asleep
  [1518-11-03 00:12] falls asleep
  [1518-08-11 00:37] wakes up
  [1518-09-09 00:46] falls asleep
  [1518-06-27 00:02] Guard #853 begins shift
  [1518-05-12 00:57] wakes up
  [1518-08-12 00:47] wakes up
  [1518-10-05 00:50] wakes up
  [1518-04-07 00:11] falls asleep
  [1518-06-26 00:09] falls asleep
  [1518-09-13 00:28] falls asleep
  [1518-04-17 23:57] Guard #379 begins shift
  [1518-04-18 00:44] falls asleep
  [1518-06-07 00:17] wakes up
  [1518-11-09 00:18] falls asleep
  [1518-08-21 00:02] Guard #1949 begins shift
  [1518-05-10 00:43] falls asleep
  [1518-08-20 00:41] falls asleep
  [1518-10-08 00:56] falls asleep
  [1518-10-02 00:46] wakes up
  [1518-08-08 00:26] wakes up
  [1518-09-04 00:56] wakes up
  [1518-08-21 23:56] Guard #2029 begins shift
  [1518-10-27 00:01] Guard #1847 begins shift
  [1518-03-29 00:02] Guard #3529 begins shift
  [1518-04-21 23:58] Guard #2699 begins shift
  [1518-05-22 00:57] wakes up
  [1518-08-29 00:37] wakes up
  [1518-11-15 00:47] wakes up
  [1518-07-29 00:53] wakes up
  [1518-09-13 00:00] Guard #2699 begins shift
  [1518-08-22 00:35] falls asleep
  [1518-11-17 00:01] falls asleep
  [1518-07-04 23:57] Guard #3373 begins shift
  [1518-07-15 00:07] falls asleep
  [1518-05-30 00:07] falls asleep
  [1518-05-23 23:57] Guard #641 begins shift
  [1518-06-01 00:20] falls asleep
  [1518-04-29 23:59] Guard #3373 begins shift
  [1518-05-15 00:42] falls asleep
  [1518-10-07 00:23] falls asleep
  [1518-10-22 00:00] Guard #1973 begins shift
  [1518-10-05 00:05] falls asleep
  [1518-05-21 00:00] Guard #1049 begins shift
  [1518-07-20 00:29] falls asleep
  [1518-07-24 00:58] wakes up
  [1518-04-17 00:58] wakes up
  [1518-06-16 23:46] Guard #193 begins shift
  [1518-04-16 00:53] falls asleep
  [1518-10-09 23:57] Guard #317 begins shift
  [1518-09-18 23:50] Guard #379 begins shift
  [1518-11-04 00:00] falls asleep
  [1518-04-28 00:46] wakes up
  [1518-05-20 00:42] falls asleep
  [1518-08-30 00:58] wakes up
  [1518-04-16 23:49] Guard #2281 begins shift
  [1518-06-28 00:06] wakes up
  [1518-10-08 00:57] wakes up
  [1518-11-10 00:53] wakes up
  [1518-10-20 00:31] wakes up
  [1518-11-08 00:56] falls asleep
  [1518-10-29 00:40] wakes up
  [1518-06-29 00:03] Guard #2281 begins shift
  [1518-11-12 00:36] wakes up
  [1518-05-21 00:27] wakes up
  [1518-07-20 23:52] Guard #2281 begins shift
  [1518-10-25 23:57] Guard #2137 begins shift
  [1518-07-25 00:15] falls asleep
  [1518-04-10 23:54] Guard #317 begins shift
  [1518-03-29 00:42] wakes up
  [1518-09-19 00:05] falls asleep
  [1518-11-22 00:33] falls asleep
  [1518-03-26 00:37] wakes up
  [1518-10-02 23:58] Guard #317 begins shift
  [1518-08-14 00:57] wakes up
  [1518-11-12 00:18] falls asleep
  [1518-03-27 00:57] falls asleep
  [1518-08-27 00:32] wakes up
  [1518-09-29 00:12] wakes up
  [1518-11-06 00:53] wakes up
  [1518-08-22 23:57] Guard #641 begins shift
  [1518-08-30 00:01] Guard #3529 begins shift
  [1518-09-22 00:16] wakes up
  [1518-03-19 23:57] Guard #641 begins shift
  [1518-04-29 00:47] wakes up
  [1518-10-08 00:51] wakes up
  [1518-11-14 00:59] wakes up
  [1518-04-22 00:58] wakes up
  [1518-06-01 00:02] Guard #1049 begins shift
  [1518-04-15 00:28] wakes up
  [1518-10-30 00:07] falls asleep
  [1518-06-22 00:40] wakes up
  [1518-05-09 00:02] falls asleep
  [1518-11-06 00:01] Guard #2281 begins shift
  [1518-07-06 23:57] Guard #1049 begins shift
  [1518-06-30 00:48] wakes up
  [1518-07-08 00:40] falls asleep
  [1518-09-15 00:50] falls asleep
  [1518-11-03 00:50] wakes up
  [1518-07-13 00:45] wakes up
  [1518-07-15 23:46] Guard #2179 begins shift
  [1518-05-07 00:57] wakes up
  [1518-10-23 00:43] wakes up
  [1518-08-18 00:42] falls asleep
  [1518-10-03 00:56] wakes up
  [1518-10-25 00:40] wakes up
  [1518-08-07 00:23] falls asleep
  [1518-07-08 00:50] wakes up
  [1518-05-13 00:40] falls asleep
  [1518-10-24 23:59] Guard #1973 begins shift
  [1518-05-20 00:51] falls asleep
  [1518-10-17 00:14] wakes up
  [1518-06-11 00:56] falls asleep
  [1518-09-26 00:36] wakes up
  [1518-03-28 00:57] falls asleep
  [1518-03-26 00:03] falls asleep
  [1518-06-10 00:03] Guard #2693 begins shift
  [1518-08-02 00:00] Guard #379 begins shift
  [1518-07-22 23:58] Guard #853 begins shift
  [1518-05-17 00:52] wakes up
  [1518-10-28 00:23] wakes up
  [1518-04-06 00:35] wakes up
  [1518-06-25 00:36] wakes up
  [1518-05-01 00:25] wakes up
  [1518-09-22 00:31] falls asleep
  [1518-11-13 00:46] wakes up
  [1518-05-24 00:14] falls asleep
  [1518-10-03 00:11] falls asleep
  [1518-11-05 00:07] falls asleep
  [1518-11-13 00:00] Guard #1973 begins shift
  [1518-08-24 00:54] falls asleep
  [1518-07-19 00:52] falls asleep
  [1518-04-09 00:04] falls asleep
  [1518-10-20 00:40] falls asleep
  [1518-05-13 23:59] Guard #2029 begins shift
  [1518-08-30 00:36] falls asleep
  [1518-06-22 00:04] Guard #2693 begins shift
  [1518-11-23 00:44] wakes up
  [1518-06-06 00:24] wakes up
  [1518-09-09 00:00] Guard #2281 begins shift
  [1518-04-27 00:42] wakes up
  [1518-04-12 00:56] falls asleep
  [1518-11-01 00:22] falls asleep
  [1518-05-11 00:55] wakes up
  [1518-06-27 00:24] falls asleep
  [1518-10-05 00:39] falls asleep
  [1518-06-17 00:26] falls asleep
  [1518-05-28 00:26] falls asleep
  [1518-06-20 00:54] falls asleep
  [1518-03-29 00:55] wakes up
  [1518-03-21 00:54] falls asleep
  [1518-06-01 23:58] Guard #509 begins shift
  [1518-05-19 00:57] falls asleep
  [1518-07-20 00:41] wakes up
  [1518-06-12 00:02] Guard #1973 begins shift
  [1518-03-23 00:57] wakes up
  [1518-10-15 00:00] Guard #2693 begins shift
  [1518-10-18 00:02] Guard #401 begins shift
  [1518-09-28 23:51] Guard #3373 begins shift
  [1518-06-20 00:10] falls asleep
  [1518-07-23 00:36] wakes up
  [1518-06-05 23:59] Guard #853 begins shift
  [1518-03-27 00:20] falls asleep
  [1518-11-14 00:11] wakes up
  [1518-08-11 23:52] Guard #401 begins shift
  [1518-04-28 00:02] Guard #317 begins shift
  [1518-04-16 00:35] falls asleep
  [1518-04-15 00:57] wakes up
  [1518-09-07 00:50] falls asleep
  [1518-08-19 00:02] Guard #193 begins shift
  [1518-09-17 00:49] falls asleep
  [1518-10-07 23:56] Guard #509 begins shift
  [1518-09-27 00:09] falls asleep
  [1518-08-22 00:49] wakes up
  [1518-03-27 00:48] falls asleep
  [1518-10-16 00:02] Guard #3529 begins shift
  [1518-05-12 00:27] falls asleep
  [1518-05-26 23:56] Guard #2693 begins shift
  [1518-10-28 00:06] falls asleep
  [1518-11-02 00:36] falls asleep
  [1518-04-18 00:59] wakes up
  [1518-07-21 00:28] wakes up
  [1518-08-28 23:58] Guard #379 begins shift
  [1518-08-17 00:17] falls asleep
  [1518-11-22 00:40] wakes up
  [1518-10-19 00:06] falls asleep
  [1518-10-11 23:56] Guard #1877 begins shift
  [1518-09-15 00:56] wakes up
  [1518-08-16 00:59] wakes up
  [1518-06-14 00:06] falls asleep
  [1518-11-10 00:14] falls asleep
  [1518-08-01 00:34] falls asleep
  [1518-06-27 00:57] wakes up
  [1518-04-22 00:53] wakes up
  [1518-04-09 00:24] wakes up
  [1518-09-30 00:36] falls asleep
  [1518-11-19 23:59] Guard #1973 begins shift
  [1518-05-03 00:31] wakes up
  [1518-09-25 00:35] wakes up
  [1518-09-21 23:50] Guard #1973 begins shift
  [1518-11-18 00:01] Guard #641 begins shift
  [1518-10-28 00:31] falls asleep
  [1518-04-28 00:57] wakes up
  [1518-10-05 00:23] wakes up
  [1518-10-28 00:55] wakes up
  [1518-08-30 00:33] wakes up
  [1518-07-16 00:00] falls asleep
  [1518-09-27 23:49] Guard #317 begins shift
  [1518-08-16 00:16] falls asleep
  [1518-03-22 00:42] falls asleep
  [1518-06-28 00:03] falls asleep
  [1518-11-16 00:28] falls asleep
  [1518-04-16 00:38] wakes up
  [1518-08-31 00:25] falls asleep
  [1518-09-30 00:04] Guard #641 begins shift
  [1518-10-13 00:02] falls asleep
  [1518-06-16 00:01] Guard #2011 begins shift
  [1518-07-26 23:59] Guard #317 begins shift
  [1518-07-15 00:26] wakes up
  [1518-08-01 00:59] wakes up
  [1518-04-17 00:01] falls asleep
  [1518-07-10 00:51] wakes up
  [1518-04-23 00:24] falls asleep
  [1518-10-05 23:56] Guard #2029 begins shift
  [1518-10-10 00:19] falls asleep
  [1518-09-01 23:57] Guard #2281 begins shift
  [1518-05-25 23:56] Guard #379 begins shift
  [1518-10-18 00:20] falls asleep
  [1518-03-30 23:58] Guard #317 begins shift
  [1518-09-29 00:54] wakes up
  [1518-04-07 00:08] wakes up
  [1518-06-09 00:56] wakes up
  [1518-06-07 00:15] falls asleep
  [1518-10-02 00:43] falls asleep
  [1518-08-30 00:29] falls asleep
  [1518-07-16 00:42] wakes up
  [1518-07-08 00:54] falls asleep
  [1518-11-21 00:59] wakes up
  [1518-10-25 00:50] falls asleep
  [1518-08-14 00:28] wakes up
  [1518-07-10 00:16] falls asleep
  [1518-05-17 00:51] falls asleep
  [1518-09-04 00:53] falls asleep
  [1518-09-26 00:01] Guard #2179 begins shift
  [1518-03-25 00:47] wakes up
  [1518-04-01 00:31] wakes up
  [1518-03-30 00:52] wakes up
  [1518-03-25 23:53] Guard #2179 begins shift
  [1518-10-23 00:16] falls asleep
  [1518-09-24 00:30] falls asleep
  [1518-09-27 00:03] Guard #2699 begins shift
  [1518-09-05 00:00] Guard #2137 begins shift
  [1518-06-25 00:50] wakes up
  [1518-05-19 00:00] Guard #2179 begins shift
  [1518-10-13 00:10] wakes up
  [1518-03-29 23:58] Guard #1877 begins shift
  [1518-03-29 00:06] falls asleep
  [1518-09-23 00:59] wakes up
  [1518-06-09 00:02] Guard #509 begins shift
  [1518-05-31 00:03] Guard #193 begins shift
  [1518-05-09 00:37] wakes up
  [1518-09-11 00:30] falls asleep
  [1518-11-22 00:58] wakes up
  [1518-04-11 00:56] wakes up
  [1518-11-19 00:09] falls asleep
  [1518-03-16 00:59] wakes up
  [1518-09-07 00:00] Guard #1049 begins shift
  [1518-05-28 00:02] Guard #509 begins shift
  [1518-10-14 00:54] wakes up
  [1518-09-17 00:01] Guard #401 begins shift
  [1518-06-07 00:42] wakes up
  [1518-03-16 00:53] wakes up
  [1518-07-25 00:53] wakes up
  [1518-05-19 00:40] wakes up
  [1518-08-25 00:10] falls asleep
  [1518-05-20 00:33] falls asleep
  [1518-09-10 00:25] wakes up
  [1518-11-23 00:31] falls asleep
  [1518-10-11 00:12] falls asleep
  [1518-07-29 00:51] falls asleep
  [1518-08-08 00:37] falls asleep
  [1518-08-06 00:58] wakes up
  [1518-05-31 00:54] wakes up
  [1518-05-07 00:47] wakes up
  [1518-06-28 00:32] falls asleep
  [1518-11-11 00:22] falls asleep
  [1518-10-26 00:37] falls asleep
  [1518-04-27 00:52] falls asleep
  [1518-07-30 23:50] Guard #401 begins shift
  [1518-04-16 00:12] wakes up
  [1518-08-11 00:01] falls asleep
  [1518-10-29 00:21] falls asleep
  [1518-09-23 00:01] Guard #853 begins shift
  [1518-07-13 23:57] Guard #2179 begins shift
  [1518-06-14 23:59] Guard #3529 begins shift
  [1518-07-29 00:02] Guard #2029 begins shift
  [1518-08-04 00:38] falls asleep
  [1518-05-06 00:59] wakes up
  [1518-04-12 00:59] wakes up
  [1518-11-16 00:54] wakes up
  [1518-08-04 00:01] Guard #1973 begins shift
  [1518-10-31 00:01] Guard #1847 begins shift
  [1518-08-23 23:59] Guard #3529 begins shift
  [1518-05-08 00:51] falls asleep
  [1518-09-01 00:13] falls asleep
  [1518-05-07 00:02] Guard #1877 begins shift
  [1518-04-09 00:55] falls asleep
  [1518-06-29 00:45] falls asleep
  [1518-09-16 00:20] wakes up
  [1518-08-21 00:52] wakes up
  [1518-04-25 00:34] falls asleep
  [1518-07-06 00:56] wakes up
  [1518-09-22 00:03] falls asleep
  [1518-09-27 00:17] wakes up
  [1518-08-19 00:51] wakes up
  [1518-11-09 23:58] Guard #641 begins shift
  [1518-09-05 23:49] Guard #853 begins shift
  [1518-11-11 00:42] falls asleep
  [1518-04-03 00:37] falls asleep
  [1518-09-08 00:31] falls asleep
  [1518-06-03 00:03] Guard #2137 begins shift
  [1518-06-26 00:28] wakes up
  [1518-06-07 00:20] falls asleep
  [1518-05-13 00:55] falls asleep
  [1518-04-24 00:48] wakes up
  [1518-05-30 00:45] wakes up
  [1518-07-19 00:02] Guard #2699 begins shift
  [1518-05-05 00:04] Guard #2029 begins shift
  [1518-07-31 00:03] falls asleep
  [1518-11-14 00:51] wakes up
  [1518-09-09 00:50] wakes up
  [1518-11-19 00:59] wakes up
  [1518-11-01 00:43] wakes up
  [1518-08-10 00:55] wakes up
  [1518-08-20 00:25] wakes up
  [1518-03-31 00:10] falls asleep
  [1518-07-26 00:54] falls asleep
  [1518-08-08 23:58] Guard #1973 begins shift
  [1518-08-07 00:58] wakes up
  [1518-05-18 00:00] Guard #853 begins shift
  [1518-11-11 00:57] wakes up
  [1518-03-21 00:41] falls asleep
  [1518-05-29 23:56] Guard #2029 begins shift
  [1518-09-24 00:51] wakes up
  [1518-09-20 00:50] wakes up
  [1518-11-18 00:44] wakes up
  [1518-05-14 23:58] Guard #317 begins shift
  [1518-10-04 23:53] Guard #1049 begins shift
  [1518-08-15 00:20] falls asleep
  [1518-05-25 00:56] wakes up
  [1518-04-19 00:42] wakes up
  [1518-07-09 00:06] falls asleep
  [1518-04-14 00:15] falls asleep
  [1518-08-03 00:56] wakes up
  [1518-04-18 00:46] wakes up
  [1518-04-22 00:33] falls asleep
  [1518-10-15 00:26] falls asleep
  [1518-10-01 00:17] falls asleep
  [1518-04-04 00:59] wakes up
  [1518-06-05 00:35] wakes up
  [1518-04-11 23:50] Guard #1049 begins shift
  [1518-07-07 00:50] wakes up
  [1518-09-03 00:56] wakes up
  [1518-03-16 00:04] Guard #1973 begins shift
  [1518-08-26 00:44] falls asleep
  [1518-10-16 00:41] falls asleep
  [1518-07-06 00:27] falls asleep
  [1518-09-04 00:40] wakes up
  [1518-10-03 00:35] wakes up
  [1518-04-26 00:28] falls asleep
  [1518-09-04 00:02] falls asleep
  [1518-04-07 00:02] Guard #2029 begins shift
  [1518-10-12 00:17] falls asleep
  [1518-07-04 00:02] Guard #2137 begins shift
  [1518-06-30 00:17] falls asleep
  [1518-04-10 00:57] wakes up
  [1518-04-06 00:24] falls asleep
  [1518-10-30 00:38] wakes up
  [1518-05-21 00:16] falls asleep
  [1518-07-09 00:54] wakes up
  [1518-07-27 00:34] falls asleep
  [1518-06-16 00:44] falls asleep
  [1518-05-10 00:02] Guard #193 begins shift
  [1518-09-25 00:03] Guard #2179 begins shift
  [1518-03-20 00:49] wakes up
  [1518-06-17 00:00] falls asleep
  [1518-03-22 00:04] Guard #1877 begins shift
  [1518-04-02 00:44] wakes up
  [1518-03-27 00:52] wakes up
  [1518-09-07 00:59] wakes up
  [1518-08-27 00:11] falls asleep
  [1518-10-20 00:58] wakes up
  [1518-09-20 00:29] falls asleep
  [1518-05-20 00:44] wakes up
  [1518-10-09 00:02] Guard #193 begins shift
  [1518-06-30 23:57] Guard #2693 begins shift
  [1518-03-19 00:04] falls asleep
  [1518-04-10 00:48] falls asleep
  [1518-08-19 00:30] falls asleep
  [1518-07-18 00:57] falls asleep
  [1518-03-22 00:54] wakes up
  [1518-06-14 00:04] Guard #641 begins shift
  [1518-03-18 00:38] falls asleep
  [1518-08-10 00:17] falls asleep
  [1518-08-13 00:37] falls asleep
  [1518-05-26 00:48] wakes up
  [1518-04-05 00:16] falls asleep
  [1518-10-11 00:43] wakes up
  [1518-11-21 00:30] falls asleep
  [1518-08-25 23:57] Guard #193 begins shift
  [1518-04-26 23:56] Guard #1973 begins shift
  [1518-10-10 00:20] wakes up
  [1518-06-25 00:53] falls asleep
  [1518-07-17 23:58] Guard #1973 begins shift
  [1518-04-02 00:30] wakes up
  [1518-04-26 00:00] Guard #1877 begins shift
  [1518-07-02 00:36] wakes up
  [1518-06-24 00:51] wakes up
  [1518-06-15 00:55] wakes up
  [1518-07-23 00:51] falls asleep
  [1518-05-30 00:15] wakes up
  [1518-05-02 00:57] wakes up
  [1518-09-27 00:42] falls asleep
  [1518-06-06 23:59] Guard #1949 begins shift
  [1518-09-14 23:57] Guard #1973 begins shift
  [1518-10-01 23:58] Guard #1949 begins shift
  [1518-08-26 00:49] wakes up
  [1518-03-28 00:59] wakes up
  [1518-04-21 00:03] Guard #3203 begins shift
  [1518-11-01 00:00] Guard #2011 begins shift
  [1518-10-26 00:50] wakes up
  [1518-06-11 00:01] Guard #401 begins shift
  [1518-10-04 00:02] Guard #2011 begins shift
  [1518-11-16 23:53] Guard #3529 begins shift
  [1518-07-16 23:56] Guard #2281 begins shift
  [1518-05-06 00:00] Guard #2137 begins shift
  [1518-10-09 00:55] wakes up
  [1518-05-13 00:58] wakes up
  [1518-06-20 00:16] wakes up
  [1518-03-29 00:47] falls asleep
  [1518-03-23 00:27] falls asleep
  [1518-06-17 00:28] wakes up
  [1518-07-27 00:54] wakes up
  [1518-11-09 00:23] wakes up
  [1518-09-11 23:56] Guard #317 begins shift
  [1518-07-09 00:02] Guard #379 begins shift
  [1518-09-05 00:57] wakes up
  [1518-03-20 23:56] Guard #1049 begins shift
  [1518-11-05 00:01] Guard #317 begins shift
  [1518-05-04 00:00] Guard #379 begins shift
  [1518-04-09 23:57] Guard #2281 begins shift
  [1518-04-05 00:20] wakes up
  [1518-09-15 00:36] falls asleep
  [1518-10-06 00:28] falls asleep
  [1518-06-21 00:02] Guard #1847 begins shift
  [1518-09-19 23:56] Guard #853 begins shift
  [1518-04-28 00:33] falls asleep
  [1518-09-12 00:12] falls asleep
  [1518-05-20 00:57] wakes up
  [1518-04-29 00:01] Guard #2281 begins shift
  [1518-06-09 00:40] wakes up
  [1518-09-08 00:47] wakes up
  [1518-05-23 00:58] wakes up
  [1518-04-24 23:59] Guard #2011 begins shift
  [1518-09-12 00:44] wakes up
  [1518-10-29 23:56] Guard #2137 begins shift
  [1518-06-19 00:52] wakes up
  [1518-07-08 00:01] Guard #401 begins shift
  [1518-08-31 00:57] wakes up
  [1518-08-28 00:03] Guard #2693 begins shift
  [1518-05-04 00:18] falls asleep
  [1518-04-10 00:45] wakes up
  [1518-07-26 00:00] Guard #193 begins shift
  [1518-04-05 00:33] falls asleep
  [1518-03-29 00:12] wakes up
  [1518-05-05 00:51] falls asleep
  [1518-05-22 00:00] Guard #509 begins shift
  [1518-05-05 00:56] wakes up
  [1518-09-01 00:18] wakes up
  [1518-03-25 00:45] falls asleep
  [1518-09-28 00:02] falls asleep
  [1518-06-23 00:54] wakes up
  [1518-05-07 00:30] falls asleep
  [1518-11-08 23:59] Guard #1949 begins shift
  [1518-03-16 00:39] wakes up
  [1518-10-02 00:39] wakes up
  [1518-06-19 00:50] falls asleep
  [1518-03-24 00:45] wakes up
  [1518-09-06 00:01] falls asleep
  [1518-04-27 00:55] wakes up
  [1518-08-20 00:08] falls asleep
  [1518-04-13 00:10] wakes up
  [1518-05-19 23:59] Guard #1973 begins shift
  [1518-10-14 00:40] wakes up
  [1518-06-30 00:00] Guard #1973 begins shift
  [1518-06-13 00:01] falls asleep
  [1518-07-14 00:45] falls asleep
  [1518-03-20 00:32] falls asleep
  [1518-07-07 00:24] falls asleep
  [1518-07-24 00:47] falls asleep
  [1518-08-25 00:58] wakes up
  [1518-09-21 00:21] falls asleep
  [1518-06-11 00:58] wakes up
  [1518-04-11 00:00] falls asleep
  [1518-10-22 00:39] wakes up
  [1518-09-23 23:58] Guard #2029 begins shift
  [1518-03-30 00:10] falls asleep
  [1518-07-01 00:06] falls asleep
  [1518-09-09 23:57] Guard #641 begins shift
  [1518-03-28 00:53] wakes up
  [1518-04-30 23:58] Guard #2179 begins shift
  [1518-07-02 00:43] wakes up
  [1518-06-04 00:01] Guard #509 begins shift
  [1518-10-01 00:24] wakes up
  [1518-06-25 00:30] falls asleep
  [1518-05-10 00:52] wakes up
  [1518-05-28 00:44] wakes up
  [1518-04-15 00:00] Guard #379 begins shift
  [1518-05-15 00:55] wakes up
  [1518-07-16 00:58] wakes up
  [1518-10-04 00:39] falls asleep
  [1518-08-25 00:46] falls asleep
  [1518-07-17 00:49] falls asleep
  [1518-09-03 00:54] falls asleep
  [1518-11-14 00:47] falls asleep
  [1518-07-15 00:03] Guard #641 begins shift
  [1518-05-15 00:53] falls asleep
  [1518-03-23 00:56] falls asleep
  [1518-09-30 23:59] Guard #1877 begins shift
  [1518-07-18 00:36] falls asleep
  [1518-09-29 00:29] falls asleep
  [1518-06-09 00:30] falls asleep
  [1518-10-18 00:42] wakes up
  [1518-05-13 00:03] Guard #509 begins shift
  [1518-11-08 00:59] wakes up
  [1518-03-16 00:52] falls asleep
  [1518-09-01 00:23] falls asleep
  [1518-07-30 00:04] falls asleep
  [1518-07-02 00:01] Guard #641 begins shift
  [1518-07-05 00:39] falls asleep
  [1518-06-25 00:57] wakes up
  [1518-08-07 23:54] Guard #641 begins shift
  [1518-03-22 00:18] falls asleep
  [1518-04-20 00:04] Guard #2179 begins shift
  [1518-09-07 00:34] falls asleep
  [1518-07-16 00:49] falls asleep
  [1518-05-10 23:57] Guard #641 begins shift
  [1518-07-31 23:56] Guard #3529 begins shift
  [1518-11-02 00:40] wakes up
  [1518-08-28 00:58] wakes up
  [1518-08-08 00:05] falls asleep
  [1518-05-27 00:50] wakes up
  [1518-04-16 00:08] falls asleep
  [1518-09-12 00:24] wakes up
  [1518-08-03 00:30] falls asleep
  [1518-09-17 23:56] Guard #1847 begins shift
  [1518-04-03 00:41] wakes up
  [1518-09-14 00:00] Guard #3529 begins shift
  [1518-04-18 00:25] falls asleep
  [1518-05-28 23:52] Guard #193 begins shift
  [1518-07-20 00:03] Guard #1949 begins shift
  [1518-08-17 00:01] falls asleep
  [1518-05-02 00:54] falls asleep
  [1518-09-10 00:17] falls asleep
  [1518-06-15 00:12] falls asleep
  [1518-08-05 00:00] Guard #3529 begins shift
  [1518-08-02 23:59] Guard #853 begins shift
  [1518-07-28 00:00] Guard #3373 begins shift
  [1518-04-25 00:53] wakes up
  [1518-07-31 00:30] wakes up
  [1518-06-07 23:47] Guard #641 begins shift
  [1518-09-13 00:58] wakes up
  [1518-04-20 00:39] wakes up
  [1518-07-28 00:32] wakes up
  [1518-04-28 00:53] falls asleep
  [1518-11-18 00:18] falls asleep
  [1518-04-17 00:48] wakes up
  [1518-05-11 00:36] falls asleep
  [1518-06-02 00:58] wakes up
  [1518-09-14 00:51] wakes up
  [1518-03-29 00:51] falls asleep
  [1518-04-08 23:47] Guard #641 begins shift
  [1518-06-08 00:01] falls asleep
  [1518-11-06 00:41] falls asleep
  [1518-08-24 00:50] wakes up
  [1518-06-02 00:52] falls asleep
  [1518-09-27 00:54] wakes up
  [1518-06-28 00:43] wakes up
  [1518-06-12 00:45] wakes up
  [1518-09-16 00:00] Guard #2029 begins shift
  [1518-05-22 23:57] Guard #379 begins shift
  [1518-05-23 00:06] falls asleep
  [1518-09-06 00:35] wakes up
  [1518-07-22 00:00] Guard #2693 begins shift
  [1518-05-23 00:17] wakes up
  [1518-03-18 00:43] wakes up
  [1518-11-11 00:38] wakes up
  [1518-04-07 00:40] wakes up
  [1518-06-10 00:29] wakes up
  [1518-04-01 23:56] Guard #317 begins shift
  [1518-08-04 00:45] wakes up
  [1518-08-08 00:52] wakes up
  [1518-06-16 00:20] falls asleep
  [1518-09-16 00:15] falls asleep
  [1518-04-02 00:33] falls asleep
  [1518-04-14 00:57] wakes up
  [1518-07-10 23:57] Guard #251 begins shift
  [1518-05-18 00:37] falls asleep
  [1518-11-14 00:54] falls asleep
  [1518-05-04 00:46] wakes up
  [1518-06-03 00:28] wakes up
  [1518-11-05 00:19] wakes up
  [1518-06-22 00:10] falls asleep
  [1518-04-15 00:35] falls asleep
  [1518-05-07 23:59] Guard #509 begins shift
  [1518-04-09 00:59] wakes up
  [1518-06-09 00:50] falls asleep
  [1518-10-22 00:33] falls asleep
  [1518-09-11 00:50] wakes up
  [1518-11-03 00:00] Guard #1877 begins shift
  [1518-06-18 00:08] falls asleep
  [1518-03-26 23:59] Guard #1973 begins shift
  [1518-06-18 00:39] wakes up
  [1518-08-15 00:00] Guard #853 begins shift
  [1518-07-01 00:42] wakes up
  [1518-05-19 00:46] falls asleep
  [1518-08-26 00:32] falls asleep
  [1518-05-29 00:03] wakes up
  [1518-04-01 00:05] falls asleep
  [1518-10-19 00:00] Guard #193 begins shift
  [1518-07-09 00:39] falls asleep
  [1518-10-15 00:52] wakes up
  [1518-09-13 00:50] falls asleep
  [1518-07-05 00:55] wakes up
  [1518-10-12 00:57] wakes up
  [1518-05-03 00:01] Guard #641 begins shift
  [1518-08-05 00:52] falls asleep
  [1518-03-17 00:55] wakes up
  [1518-08-17 00:06] wakes up
  [1518-04-02 00:23] falls asleep
  [1518-07-29 23:46] Guard #2693 begins shift
  [1518-04-27 00:37] falls asleep
  [1518-05-19 00:37] falls asleep
  [1518-10-08 00:37] falls asleep
  [1518-11-07 00:59] wakes up
  [1518-10-10 23:56] Guard #379 begins shift
  [1518-06-20 00:04] Guard #3373 begins shift
  [1518-06-12 00:28] falls asleep
  [1518-09-29 00:00] falls asleep
  [1518-09-23 00:50] wakes up
  [1518-11-16 00:07] falls asleep
  [1518-09-15 00:38] wakes up
  [1518-11-07 00:42] falls asleep
  [1518-08-28 00:18] falls asleep
  [1518-06-29 00:26] wakes up
  [1518-06-06 00:50] falls asleep
  [1518-04-23 00:59] wakes up
  [1518-03-23 00:44] wakes up
  [1518-06-02 00:33] falls asleep
  [1518-08-07 00:19] wakes up
  [1518-03-21 00:44] wakes up
  [1518-09-23 00:29] falls asleep
  [1518-08-17 23:59] Guard #1049 begins shift
  [1518-07-18 00:41] wakes up
  [1518-10-23 23:57] Guard #251 begins shift
  [1518-10-16 23:59] Guard #401 begins shift
  [1518-10-09 00:15] falls asleep
  [1518-04-08 00:41] wakes up
  [1518-09-03 00:04] Guard #3373 begins shift
  [1518-06-04 00:57] wakes up
  [1518-10-25 00:58] wakes up
  [1518-09-02 00:51] wakes up
  [1518-07-22 00:55] wakes up
  [1518-07-24 00:36] wakes up
  [1518-09-06 00:59] wakes up
  [1518-05-01 00:12] falls asleep
  [1518-09-21 00:48] wakes up
  [1518-03-16 23:57] Guard #1949 begins shift
  [1518-05-23 00:26] falls asleep
  [1518-04-16 00:58] wakes up
  [1518-08-16 23:50] Guard #1949 begins shift
  [1518-04-12 23:56] Guard #193 begins shift
  [1518-09-01 00:54] wakes up
  [1518-08-12 00:02] falls asleep
  [1518-04-06 00:26] wakes up
  [1518-09-13 00:38] wakes up
  [1518-04-24 00:40] falls asleep
  [1518-08-02 00:18] falls asleep
  [1518-09-05 00:24] falls asleep
  [1518-04-11 00:30] wakes up
  [1518-10-09 00:19] wakes up
  [1518-08-02 00:41] wakes up
  [1518-11-21 00:46] falls asleep
  [1518-10-29 00:00] Guard #3529 begins shift
  [1518-11-21 00:00] Guard #1973 begins shift
  [1518-10-30 00:46] falls asleep
  [1518-07-26 00:55] wakes up
  [1518-06-25 00:48] falls asleep
  [1518-03-28 00:01] Guard #379 begins shift
  [1518-07-15 00:58] wakes up
  [1518-04-12 00:27] wakes up
  [1518-06-17 23:58] Guard #193 begins shift
  [1518-05-18 00:57] falls asleep
  [1518-03-17 00:46] falls asleep
  [1518-08-21 00:37] falls asleep
  [1518-10-25 00:35] falls asleep
  [1518-09-30 00:53] wakes up
  [1518-05-20 00:38] wakes up
  [1518-03-29 00:48] wakes up
  [1518-11-22 00:57] falls asleep
  [1518-11-16 00:00] Guard #3373 begins shift
  [1518-07-12 23:58] Guard #2029 begins shift
  [1518-07-23 00:28] falls asleep
  [1518-05-13 00:49] wakes up
  [1518-08-31 00:03] Guard #1973 begins shift
  [1518-04-04 00:35] falls asleep
  [1518-07-10 00:03] Guard #1049 begins shift
  [1518-09-23 00:54] falls asleep
  [1518-11-10 23:58] Guard #509 begins shift
  [1518-04-17 00:51] falls asleep
  [1518-06-19 00:33] wakes up
  [1518-06-08 00:38] falls asleep
  [1518-09-28 00:47] wakes up
  [1518-06-01 00:51] falls asleep
  [1518-05-27 00:20] falls asleep
  [1518-08-23 00:30] falls asleep
  [1518-10-20 00:02] falls asleep
  [1518-04-29 00:26] falls asleep
  [1518-10-09 00:53] falls asleep
  [1518-08-07 00:10] falls asleep
  [1518-04-05 00:51] wakes up
  [1518-09-08 00:10] falls asleep
  [1518-08-25 00:03] Guard #317 begins shift
  [1518-04-19 00:03] Guard #1049 begins shift
  [1518-08-13 00:57] wakes up
  [1518-05-28 00:56] wakes up
  [1518-06-04 23:47] Guard #2137 begins shift
  [1518-10-06 00:36] wakes up
  [1518-04-07 23:58] Guard #317 begins shift
  [1518-06-26 00:00] Guard #641 begins shift
  [1518-09-25 00:20] falls asleep
  [1518-06-23 00:12] falls asleep
  [1518-05-08 23:46] Guard #3529 begins shift
  [1518-07-04 00:45] wakes up
  [1518-07-13 00:11] falls asleep
  [1518-05-11 23:57] Guard #509 begins shift
  [1518-06-17 00:11] wakes up
  [1518-08-09 23:58] Guard #401 begins shift
  [1518-07-06 00:00] Guard #3373 begins shift
  [1518-05-08 00:58] wakes up
  [1518-10-12 23:49] Guard #2281 begins shift
  [1518-08-18 00:58] wakes up
  [1518-08-15 00:58] wakes up
  [1518-09-05 00:47] falls asleep
  [1518-07-22 00:20] falls asleep
  [1518-03-17 00:41] wakes up
  [1518-09-14 00:15] falls asleep
  [1518-03-23 23:59] Guard #1049 begins shift
  [1518-03-18 23:47] Guard #641 begins shift
  [1518-06-20 00:58] wakes up
  [1518-05-29 00:02] falls asleep
  [1518-03-31 23:51] Guard #2699 begins shift
  [1518-06-19 00:26] falls asleep
  [1518-10-06 23:59] Guard #2281 begins shift
  [1518-07-28 00:09] falls asleep
  [1518-08-14 00:32] falls asleep
  [1518-03-22 23:57] Guard #1949 begins shift
  [1518-07-17 00:51] wakes up
  [1518-06-08 00:13] wakes up
  [1518-07-14 00:37] falls asleep
  [1518-06-12 23:49] Guard #2011 begins shift
  [1518-07-23 00:59] wakes up
  [1518-09-03 23:52] Guard #2029 begins shift
  [1518-08-23 00:59] wakes up
  [1518-05-26 00:32] falls asleep
  [1518-06-08 00:54] wakes up
  [1518-05-06 00:44] falls asleep
  [1518-08-06 00:05] falls asleep
  [1518-08-14 00:01] falls asleep
  [1518-04-06 00:02] Guard #2137 begins shift
  [1518-08-28 00:41] falls asleep
  [1518-06-24 00:16] falls asleep
  [1518-09-17 00:53] wakes up
  [1518-10-21 00:00] Guard #1847 begins shift
  [1518-09-08 00:17] wakes up
  [1518-06-06 00:08] falls asleep
  [1518-10-04 00:59] wakes up
  [1518-04-19 00:23] falls asleep
  [1518-06-23 00:00] Guard #2281 begins shift
  [1518-05-16 23:59] Guard #509 begins shift
  [1518-08-29 00:10] falls asleep
  [1518-05-22 00:07] falls asleep
  [1518-06-29 00:18] falls asleep
  [1518-11-16 00:24] wakes up
  [1518-05-14 00:23] falls asleep
  [1518-10-02 00:35] falls asleep
  [1518-06-16 00:24] wakes up
  [1518-08-27 00:00] Guard #379 begins shift
  [1518-04-15 00:37] wakes up
  [1518-03-28 00:39] wakes up
  [1518-10-19 23:52] Guard #2011 begins shift
  [1518-11-03 23:52] Guard #2693 begins shift
  [1518-04-16 00:25] wakes up
  [1518-07-09 00:22] wakes up
  [1518-05-19 00:47] wakes up
  [1518-03-21 00:57] wakes up
  [1518-06-06 00:57] wakes up
  [1518-06-29 00:47] wakes up
  [1518-04-11 00:42] falls asleep
  [1518-05-30 00:19] falls asleep
  [1518-08-10 23:50] Guard #2179 begins shift
  [1518-03-22 00:24] wakes up
  [1518-10-14 00:46] falls asleep
  [1518-08-20 00:59] wakes up
  [1518-04-06 00:40] falls asleep
  [1518-07-24 00:35] falls asleep
  [1518-08-17 00:48] wakes up
  [1518-04-20 00:08] falls asleep
  [1518-04-04 23:56] Guard #379 begins shift
  [1518-08-16 00:00] Guard #193 begins shift
  [1518-07-03 00:02] Guard #251 begins shift
  [1518-05-24 00:42] wakes up
  [1518-09-06 00:44] falls asleep
  [1518-07-14 00:49] wakes up
  [1518-07-12 00:02] Guard #193 begins shift
  [1518-05-31 00:53] falls asleep
  [1518-04-03 00:00] Guard #2699 begins shift
  [1518-07-15 00:38] falls asleep
  [1518-07-21 00:05] falls asleep
  [1518-05-03 00:41] falls asleep
  [1518-06-12 00:50] falls asleep
  [1518-04-04 00:04] Guard #1949 begins shift
  [1518-03-24 00:08] falls asleep
  [1518-04-24 00:02] Guard #1049 begins shift
  [1518-04-15 00:22] falls asleep
  [1518-05-15 00:50] wakes up
  [1518-11-15 00:11] falls asleep
  [1518-04-06 00:31] falls asleep
  [1518-04-23 00:00] Guard #3373 begins shift
  [1518-05-18 00:53] wakes up
  [1518-06-02 00:35] wakes up
  [1518-10-23 00:02] Guard #2699 begins shift
  [1518-07-12 00:39] falls asleep
  [1518-03-16 00:56] falls asleep
  [1518-09-19 00:53] wakes up
  [1518-04-10 00:08] falls asleep
  [1518-08-09 00:48] wakes up
  [1518-11-20 00:49] wakes up
  [1518-06-12 00:56] wakes up
  [1518-10-19 00:48] wakes up
  [1518-07-08 00:59] wakes up
  [1518-08-24 00:08] falls asleep
  [1518-10-30 00:57] wakes up
  [1518-07-14 00:42] wakes up
  [1518-04-16 00:20] falls asleep
  [1518-09-26 00:12] falls asleep
  [1518-04-18 00:36] wakes up
  [1518-05-25 00:11] falls asleep
  [1518-06-04 00:07] falls asleep
  [1518-05-16 00:03] Guard #251 begins shift
")