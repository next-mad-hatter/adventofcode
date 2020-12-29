(ns aoc-2020.day-18-test
  (:require [aoc-2020.day-18 :as sut]
            [clojure.test :as t]))

(t/deftest calc-1-test
  (t/is (= 71 (sut/calc-1 "1 + 2 * 3 + 4 * 5 + 6")))
  (t/is (= 51 (sut/calc-1 "1 + (2 * 3) + (4 * (5 + 6))")))
  (t/is (= 26 (sut/calc-1 "2 * 3 + (4 * 5)")))
  (t/is (= 437 (sut/calc-1 "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
  (t/is (= 12240 (sut/calc-1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
  (t/is (= 13632 (sut/calc-1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))

(t/deftest calc-2-test
  (t/is (= 51 (sut/calc-2 "1 + (2 * 3) + (4 * (5 + 6))")))
  (t/is (= 231 (sut/calc-2 "1 + 2 * 3 + 4 * 5 + 6")))
  (t/is (= 46 (sut/calc-2 "2 * 3 + (4 * 5)")))
  (t/is (= 1445 (sut/calc-2 "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
  (t/is (= 669060 (sut/calc-2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
  (t/is (= 23340 (sut/calc-2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))
