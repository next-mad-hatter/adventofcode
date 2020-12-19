(ns aoc-2020.day-19-test
  (:require [aoc-2020.day-19 :as sut]
            [clojure.test :as t]))

(t/deftest part-1
  (t/is (= 2 ((sut/solver identity) "2020/day_19_test_1.txt")))
  (t/is (= 3 ((sut/solver identity) "2020/day_19_test_2.txt"))))

(t/deftest part-2
  (t/is (= 12 ((sut/solver sut/replace-rule) "2020/day_19_test_2.txt"))))
