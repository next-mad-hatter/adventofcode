(ns aoc-2020.day-11-test
  (:require [aoc-2020.day-11 :as sut]
            [clojure.test :as t]))

(t/deftest part-1
  (t/is (= 37 (sut/part-1 "2020/day_11_test.txt"))))

(t/deftest part-2
  (t/is (= 26 (sut/part-2 "2020/day_11_test.txt"))))
