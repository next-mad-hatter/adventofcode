(ns aoc-2020.day-14
  (:require [common.util :as util]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.core.match :refer [match]]))

(defn parse-line [line]
  (if-let [masking (re-matches #"mask\s*=\s+([X10]{36})" line)]
    [:mask (second masking)]
    (when-let [assignment (re-matches #"mem\[(\d+)\]\s*=\s*(\d+)" line)]
      (into [:set] (map edn/read-string (rest assignment))))))

(defn read-input [filename]
  (->> filename
       util/fetch-lines
       (map parse-line)))

(defn mask->or [mask]
  (-> mask
      (str/replace #"X" "0")
      (Long/parseLong 2)))

(defn mask->and [mask]
  (-> mask
      (str/replace #"X" "1")
      (Long/parseLong 2)))

(defn apply-masks [and-mask or-mask x]
  (-> x
      (bit-and and-mask)
      (bit-or or-mask)))

(def init-v1 {:mem    {}
              :masker identity})

(defn fw-v1 [state instr]
  (let [[op & args] instr]
    (match op
           :mask (let [m (first args)
                       o (mask->or m)
                       a (mask->and m)]
                   (assoc state :masker (partial apply-masks a o)))
           :set (let [[addr val] (vec args)
                      masked     ((state :masker) val)]
                  (assoc-in state [:mem addr] masked)))))

(defn run-prg [fw init filename]
  (->> filename
       read-input
       (reduce fw init)
       :mem
       vals
       (apply +)))

(time
 (run-prg fw-v1 init-v1 "2020/day_14_test_1.txt"))
;; => 165

(time
 (run-prg fw-v1 init-v1 "2020/day_14_input.txt"))
;; => 4886706177792

(defn mask->bit-combos [mask]
  (let [pos (keys (util/re-pos #"X" (str/reverse mask)))]
    (for [bits (combo/selections [0 1] (count pos))]
      (let [ops (mapv #(fn [x] (([bit-clear bit-set] %2) x %1)) pos bits)]
        (fn [x] (reduce #(%2 %1) x ops))))))

(defn mapper [mask]
  (let [o  (partial bit-or (mask->or mask))
        cs (mask->bit-combos mask)
        vs (map #(comp o %) cs)]
    (apply juxt vs)))

(defn fw-v2 [state instr]
  (let [[op & args] instr]
    (match op
           :mask (let [m (first args)]
                   (assoc state :mapper (mapper m)))
           :set (let [[addr val] (vec args)
                      addresses  ((state :mapper) addr)
                      mem'       (reduce #(assoc %1 %2 val) (state :mem) addresses)]
                  (assoc state :mem mem')))))

(def init-v2 {:mem    {}
              :mapper (fn [a] [a])})

(time
 (run-prg fw-v2 init-v2 "2020/day_14_test_2.txt"))
;; => 208

(time
 (run-prg fw-v2 init-v2 "2020/day_14_input.txt"))
;; => 3348493585827
