(ns aoc-2020.day-23-test
  (:require [aoc-2020.day-23 :as sut]
            [clojure.test :as t]))

(t/deftest decrease-test
  (t/is (=   4 (sut/decrease 3 5 #{1 2 5})))
  (t/is (=   1 (sut/decrease 2 5 #{4 3 2})))
  (t/is (=   5 (sut/decrease 1 5 #{3 2 1})))
  (t/is (= nil (sut/decrease 1 3 #{1 2 3}))))

(t/deftest part-1-test
  (t/is (= "125467389" (sut/part-1   0 "389125467")))
  (t/is (= "154673289" (sut/part-1   1 "389125467")))
  (t/is (= "132546789" (sut/part-1   2 "389125467")))
  (t/is (= "167384529" (sut/part-1 100 "389125467")))
  (t/is (= "175893264" (sut/part-1 100 "974618352"))))
