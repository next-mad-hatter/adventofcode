(ns aoc-2020.day-10-test
  (:require [aoc-2020.day-10 :as sut]
            ;; [clojure.math.combinatorics :as combo]
            [clojure.test :as t]))

(t/deftest complete-path
  (t/is (= [0 1] (sut/complete-path [0 1] #{})))
  (t/is (= [0 1 3 5 6 9] (sut/complete-path [0 1] #{5 3 9 6})))
  (t/is (= nil (sut/complete-path [0 1] #{0})))
  (t/is (= nil (sut/complete-path [0 1] #{5}))))

(t/deftest path-counter
  (let [c sut/path-counter]
    (t/is (= 1 (c 1 #{})))
    (t/is (= 0 (c 1 #{0})))
    (t/is (= 0 (c 1 #{5})))
    (t/is (= 1 (c 1 #{4 7})))
    (t/is (= 4 (c 1 #{2 3 4})))
    (t/is (= 3 (c 1 #{2 4 5})))))

(t/deftest part-1
  (t/is (= '(35 220 2030) (sut/part-1))))

(t/deftest part-2
  (t/is (= '(8 19208 42313823813632) (sut/part-2))))
