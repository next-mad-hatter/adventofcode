(ns aoc-2020.template
  (:require
   [aoc-2020.util :as util]
   [clojure.test :as t]
   #_[clojure.string :as str]
   #_[clojure.edn :as edn]
   #_[clojure.java.io :as io]
   #_[clojure.string :as str]
   #_[clojure.set :as set]
   #_[clojure.algo.generic.functor :as gf :refer [fmap]]
   #_[clojure.math.combinatorics :as combo]
   #_[ubergraph.core :as uber]
   #_[ubergraph.alg :as ga :refer [topsort]]
   #_[clojure.zip :as zip]
   #_[taoensso.tufte :as tufte :refer [p profile]]))

;; TODO: check out kibit

(util/fetch-whole "2020/day_01_test.txt")

(util/fetch-lines "2020/day_08_input.txt")

(util/fetch-numbers "2020/day_10_input.txt")

(util/fetch-chunks #"\R\s*\R" "2020/day_06_test.txt")

(t/deftest whooot
  (t/is (= 1 1)))

(comment
  (time
   (profile
    {}
    (get [1] 0))))
