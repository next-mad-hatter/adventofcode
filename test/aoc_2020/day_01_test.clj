(ns aoc-2020.day-01-test
  (:require [aoc-2020.day-01 :as sut]
            [common.util :as util]
            [clojure.test :as t]))

(t/deftest simple-input
  (let [input (util/fetch-numbers "2020/day_01_test.txt")]
    (t/is (= (sut/solve input 2) 514579))
    (t/is (= (sut/solve input 3) 241861950 ))))

(t/deftest real-input
  (let [input (util/fetch-numbers "2020/day_01_input.txt")]
    (t/is (= (sut/solve input 2) 888331))
    (t/is (= (sut/solve input 3) 130933530))))
