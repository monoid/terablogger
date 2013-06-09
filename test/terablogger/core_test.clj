(ns terablogger.core-test
  (:require [clojure.test :refer :all]
            [terablogger.core :refer :all]))

(deftest post-path-test1
  (testing "Basic post-path."
    (is (= "2006/04/07/T12_04_59/index.html"
           (post-path "2006-04-07T12_04_59")))))

(deftest post-path-test2
  (testing "post-path with explicit separator."
    (is (= "2006$04$07$T12_04_59$index.html"
           (post-path "$" "2006-04-07T12_04_59")))))

(deftest post-path-test3
  (testing "post-path with extension."
    (is (= "2006/04/07/T12_04_59/index.html"
           (post-path "2006-04-07T12_04_59.txt")))))


(deftest post-month-test
  (testing "post-month test."
    (is (= "2006/04"
           (month-path "2006-04-07T12_04_59.txt")))))
