(ns aoc-2020.day-13-test
  (:require [aoc-2020.day-13 :as sut]
            [clojure.test :as t]))

(t/deftest test-1
  (t/is (= 3417 (sut/solve-string "17,x,13,19"))))

(t/deftest test-2
  (t/is (= 754018 (sut/solve-string "67,7,59,61"))))

(t/deftest test-3
  (t/is (= 779210 (sut/solve-string "67,x,7,59,61"))))

(t/deftest test-4
  (t/is (= 1261476 (sut/solve-string "67,7,x,59,61"))))

(t/deftest test-5
  (t/is (= 1202161486 (sut/solve-string "1789,37,47,1889"))))

(t/deftest test-6
  (t/is (= 1068781 (sut/solve-string (sut/get-input-2 "2020/day_13_test.txt")))))
