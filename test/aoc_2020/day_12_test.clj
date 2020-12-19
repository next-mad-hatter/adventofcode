(ns aoc-2020.day-12-test
  (:require [aoc-2020.day-12 :as sut]
            [clojure.test :as t]))

(t/deftest move-test
  (t/is (= [-2 11] (sut/move [3 6] [-1 1] 5))))

(t/deftest rotate-test
  (t/is (= 3 (sut/rotate 1 :L 180))))

(t/deftest move-step-test
  (t/is (= {:pos [3 8]} (sut/step-one {:pos [3 5]} [:N 3]))))

(t/deftest move-rot-test
  (t/is (= {:dir 1} (sut/step-one {:dir 2} [:R 90]))))

(t/deftest move-fw-test
  (t/is (= {:pos [-32 5] :dir 2} (sut/step-one {:pos [3 5] :dir 2} [:F 35]))))

(t/deftest part-1
  (t/is (= 25 (sut/part-1 "2020/day_12_test.txt"))))

(t/deftest part-2
  (t/is (= 286 (sut/part-2 "2020/day_12_test.txt"))))
