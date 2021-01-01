(ns aoc-2015.template
  (:require
   [common.util :as util]
   [clojure.test :as t]
   #_[clojure.core.match :refer [match]]
   #_[clojure.string :as str]
   #_[clojure.edn :as edn]
   #_[clojure.java.io :as io]
   #_[clojure.set :as set]
   #_[clojure.algo.generic.functor :as gf :refer [fmap]]
   #_[clojure.math.combinatorics :as combo]
   #_[ubergraph.core :as uber]
   #_[ubergraph.alg :as ga :refer [topsort]]
   #_[clojure.zip :as zip]
   #_[taoensso.tufte :as tufte :refer [p profile]]))

#_(set! *warn-on-reflection* true)

;; TODO: check out kibit

(util/fetch-whole "2015/day_01_test.txt")

(util/fetch-lines "2015/day_08_input.txt")

(util/fetch-numbers "2015/day_10_input.txt")

(util/fetch-chunks #"\R\s*\R" "2015/day_06_test.txt")

(t/deftest whooot
  (t/is (= 1 1)))

(tufte/add-basic-println-handler! {})

(comment
  (time
   (profile
    {}
    (get [1] 0))))
