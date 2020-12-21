(ns aoc-2020.util
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [clojure.pprint :as pp]))

(defn fetch-whole [filename]
  (with-open [rdr (io/reader (io/resource filename))]
    (slurp rdr)))

(defn fetch-chunks [re filename]
  (str/split (fetch-whole filename) re))

(defn fetch-lines [filename]
  (with-open [rdr (io/reader (io/resource filename))]
    (->> (line-seq rdr) vec)))

(defn fetch-numbers [filename]
  (with-open [rdr (io/reader (io/resource filename))]
    (->> (line-seq rdr)
         (map edn/read-string)
         vec)))

(defn indices-of [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn find-first [f coll]
  (first (filter f coll)))

(defn spy
  ([val] (spy "DBG:" val))
  ([msg val] (print msg " ") (pp/pprint val) val))

(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))
