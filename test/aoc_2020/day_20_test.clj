(ns aoc-2020.day-20-test
  (:require [aoc-2020.day-20 :as sut]
            [clojure.test :as t]))

(def tile [[1 2 3]
           [4 5 6]])

(t/deftest tile-ops-test
  (t/is (= [[4 1]
            [5 2]
            [6 3]]
           (sut/rot tile)))
  (t/is (= [[3 2 1]
            [6 5 4]]
           (sut/flip tile))))

(t/deftest tile-boundary-test
  (t/is (= [[1 2 3] [3 6] [4 5 6] [1 4]]
           (sut/boundary-of-tile tile))))

(t/deftest tile-configs-test
  (t/is (= [[[1 2 3] [3 6] [4 5 6] [1 4]]
            [[4 1] [1 2 3] [6 3] [4 5 6]]
            [[6 5 4] [4 1] [3 2 1] [6 3]]
            [[3 6] [6 5 4] [1 4] [3 2 1]]
            [[3 2 1] [1 4] [6 5 4] [3 6]]
            [[1 4] [4 5 6] [3 6] [1 2 3]]
            [[4 5 6] [6 3] [1 2 3] [4 1]]
            [[6 3] [3 2 1] [4 1] [6 5 4]]]
           (sut/tile-configs tile))))

(t/deftest part-1-test
  (t/is (= '(110355024 15003787688423 20899048083289)
           (map (comp first sut/solve-part-1) ["2020/day_20_small.txt"
                                               "2020/day_20_input.txt"
                                               "2020/day_20_test.txt"]))))
