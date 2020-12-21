(ns aoc-2020.day-15)

;;
;; Van Eck sequence: see https://rosettacode.org/wiki/Van_Eck_sequence#Clojure
;;

;;
;; Part 1
;;

(nth (van-eck 2 2 {1 0 3 1}) 2017)
;; => 1
(nth (van-eck 3 2 {2 0 1 1}) 2017)
;; => 10
(nth (van-eck 3 2 {1 0 2 1}) 2017)
;; => 27
(nth (van-eck 1 2 {2 0 3 1}) 2017)
;; => 78
(nth (van-eck 1 2 {3 0 2 1}) 2017)
;; => 438
(nth (van-eck 2 2 {3 0 1 1}) 2017)
;; => 1836
(nth (van-eck 17 5 {14 0, 8 1, 16 2, 0 3, 1 4}) 2014)
;; => 240

;;
;; Part 2
;;

(nth (van-eck 6 2 {0 0 3 1}) (- 30000000 3))
;; => 175594
(nth (van-eck 17 5 {14 0, 8 1, 16 2, 0 3, 1 4}) (- 30000000 6))
;; => 505
