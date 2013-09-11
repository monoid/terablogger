(ns terablogger.core-test
  (:import java.io.File)
  (:require [clojure.test :refer :all]
            [terablogger.core :refer :all]
            [clojure.string :as string]))

;; Test data: categories
(def categories
  (map (partial apply parse-cat)
       ;; Categories are in reversed order to check ordering in article-cats.
       [["cat_2.db"
         "Test cat 2
2013-08-23T120000.txt"]
        ["cat_1.db"
         "Test cat 1
2013-08-23T120000.txt
2013-08-30T120000.txt"]]))

(deftest post-cats-test
  (testing "post-cats test"
    (is (= (list (nth categories 1))
           (post-cats "2013-08-30T120000.txt" categories)))))

(deftest post-cats-sort-test
  (testing "post-cats ordering"
    (is (= (reverse categories)
           (post-cats "2013-08-23T120000.txt" categories)))))

(deftest post-apath-test1
  (testing "Basic post-path."
    (is (= ["2006" "04" "07" "T12_04_59" "index.html"]
           (post-apath "2006-04-07T12_04_59")))))

(deftest post-apath-test2
  (testing "post-path with extension."
    (is (= ["2006" "04" "07" "T12_04_59" "index.html"]
           (post-apath "2006-04-07T12_04_59.txt")))))

(deftest month-apath-test
  (testing "month-apath test."
    (is (= ["2006" "04"]
           (month-apath "2006-04-07T12_04_59.txt")))))

(deftest day-apath-test
  (testing "day-apath test."
    (is (= ["2006" "04" "07"]
           (day-apath "2006-04-07T12_04_59.txt")))))

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


(deftest post-ts-test1
  (testing "post-ts"
    (is (= "2012-10-12T22:51:57Z"
           (post-ts "2012-10-12T22_51_57.txt")))))

(deftest article-id-test1
  (testing "article-id"
    (is (= "pred"
           (article-id "pred.txt")))))

(deftest sort*-default-test
  (testing "sort* with default comparator"
    (is (= [5 3 2 2 1]
           (sort* [1 2 5 3 2])))))

(deftest sort*-test
  (testing "sort* with comparator"
    (is (= [{:id 5} {:id 3} {:id 2} {:id 2} {:id 1}]
           (sort* #(compare (:id %1) (:id %2))
                  [{:id 1} {:id 2} {:id 5} {:id 3} {:id 2}])))))

(deftest href-test
  (testing "href"
    (is (= "<a href=\"http://example-blog.com/blog/test?a=b&amp;u=v\">A&gt;B&gt;C</a>"
           (href ["test?a=b&u=v"]
                 "A>B>C")))))

(deftest parse-cat-test
  (testing "parse-cat basic"
    (is (= (map->Category
            {:id "10",
             :apath ["archives" "cat_10" ""],
             :name "Test",
             :files '("2013-08-23T120000.txt" "2013-08-30T120000.txt"),
             :count 2,
             :set     #{"2013-08-30T120000.txt" "2013-08-23T120000.txt"}})
           (parse-cat "cat_10.db"
                      "Test
2013-08-23T120000.txt
2013-08-30T120000.txt
")))))

(deftest find-cat-by-id-test
  (testing "find-cat-by-id"
    (is (= (first categories)
           (binding [*cats* categories]
                    (find-cat-by-id "2"))))))
