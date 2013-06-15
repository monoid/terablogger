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

(deftest paginated-bar-test1
  (testing "short paginating bar"
    (is (= "[1]" (paginated-bar 1 1 "/blog/parts")))))


(deftest paginated-bar-test2
  (testing "short paginating bar"
    (is (= "[1] [<a rel='next' href='/blog/parts/cat_1/index-page2.html'>2</a>]"
           (paginated-bar 1 2 "/blog/parts/cat_1")))))

(deftest paginated-bar-test1-2
  (testing "short paginating bar"
    (is (= "[<a rel='prev' href='/blog/parts/cat_1/index.html'>1</a>] [2]"
           (paginated-bar 2 2 "/blog/parts/cat_1")))))

(deftest paginated-filename-test1
  (testing "First paginated filename."
    (is (= "index.html"
           (paginated-filename 1)))))

(deftest paginated-filename-test2
  (testing "Second paginated filename."
    (is (= "index-page2.html"
           (paginated-filename 2)))))

