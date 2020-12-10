(ns aoc-2020.day-10-test
  (:require [aoc-2020.day-10 :as sut]
            [clojure.test :as t]))

(t/deftest part-1
  (t/is (= (sut/part-1) '(35 220 2030))))

(t/deftest part-2
  (t/is (= (sut/part-2) '(8 19208 42313823813632))))
