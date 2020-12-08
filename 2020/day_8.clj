(ns madhat.adventofcode.day-eight
  (:require
   [clojure.string :as str]))

(defn parse-line [line]
  (let [[op num] (str/split line #"\s+")]
    {:op  (keyword op)
     :arg (Long/parseLong num)}))

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
      (let [instr        (nth prg (:ptr state))
            delta        (((:op instr) fw) (:arg instr))
            next-state   (merge-with + state delta)
            next-visited (conj visited (:ptr state))]
        (if (= visited next-visited)
          (assoc state :exit 1)
          (recur next-state next-visited))))))

(def run-vm (partial evaluate instructions))

;; Part 1

(time
 (def simulated
   (run-vm input)))

(def answer-1
   (:acc simulated))

answer-1

;; Part 2

(defn indices-of [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn find-first [f coll]
  (first (filter f coll)))

(def answer-2
  (let [ps   (indices-of #(#{:jmp :nop} (:op %)) input)
        prgs (map #(update-in input [% :op] {:nop :jmp :jmp :nop}) ps)
        res  (map run-vm prgs)]
    (:acc (find-first #(= 0 (:exit %)) res))))

answer-2

