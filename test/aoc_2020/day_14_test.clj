(ns aoc-2020.day-14-test
  (:require [aoc-2020.day-14 :as sut]
            [clojure.test :as t]))

(def test-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")

(t/deftest masking
  (t/is (=  73 (sut/apply-masks (sut/mask->and test-mask) (sut/mask->or test-mask)  11)))
  (t/is (=  64 (sut/apply-masks (sut/mask->and test-mask) (sut/mask->or test-mask)   0)))
  (t/is (= 101 (sut/apply-masks (sut/mask->and test-mask) (sut/mask->or test-mask) 101))))

(t/deftest mapping
  (t/is (= #{59 58 27 26}
           (set ((sut/mapper "000000000000000000000000000000X1001X") 42))))
  (t/is (= #{27 24 25 17 19 26 16 18}
           (set ((sut/mapper "00000000000000000000000000000000X0XX") 26)))))
