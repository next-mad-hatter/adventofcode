(ns madhat.adventofcode.day-six
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [clojure.set :as set]
   #_[clojure.algo.generic.functor :as gf :refer [fmap]]
   #_[clojure.math.combinatorics :as combo]
   #_[ubergraph.core :as uber]
   #_[ubergraph.alg :as ga :refer [topsort]]
   #_[clojure.zip :as zip]))

(defn spy
  ([val] (spy "DBG:" val))
  ([msg val] (print msg " ") (pp/pprint val) val))

(defn parse-line [line]
  (let [chunks (str/split line #"\s+")
        op     (keyword (first chunks))
        num    (Long/parseLong (first (reverse chunks)))]
    {:op op :arg num}))

(defn parse-input [input]
  (->> input
       str/lower-case
       str/split-lines
       (mapv parse-line)))

(time
 (def input
   (->> "day_8_input.txt"
        slurp
        parse-input)))

(def instructions
  {:nop #(do % {:ptr 1})
   :acc #(do {:acc % :ptr 1})
   :jmp #(do {:ptr %})})

(defn evaluate [fw prg]
  (loop [state   {:acc 0 :ptr 0}
         visited #{}]
    (if (>= (:ptr state) (count prg))
      (assoc state :exit 0)
      (let [instr       (nth prg (:ptr state))
            delta       (((:op instr) fw) (:arg instr))
            new-state   (merge-with + state delta)
            new-visited (conj visited (:ptr state))]
        (if (= visited new-visited)
          (assoc state :exit 1)
          (recur new-state new-visited))))))

(def run-vm (partial evaluate instructions))

;; Part 1

(time
 (def simulated
   (run-vm input)))

(def answer-1
   (:acc simulated))

answer-1

;; Part 2

(defn patch [prg n]
  (let [instr  (prg n)
        new-op ({:nop :jmp :jmp :nop} (:op instr))
        patch  (assoc instr :op new-op)]
    (assoc prg n patch)))

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn find-first
  [f coll]
  (first (filter f coll)))

(def answer-2
  (let [ps   (indices #(#{:jmp :nop} (:op %)) input)
        prgs (map (partial patch input) ps)
        res  (map run-vm prgs)]
    (:acc (find-first #(= 0 (:exit %)) res))))

answer-2

