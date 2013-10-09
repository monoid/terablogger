(ns terablogger.core-test
  (:import java.io.File)
  (:require [clojure.test :refer :all]
            [terablogger.core :refer :all]
            [clojure.string :as string]))

;; Test data: categories
(def categories
  (map (partial apply parse-cat)
       ;; Categories are in reversed order to check ordering in article-cats.
       [["cat_1.db"
         "Test cat 1
2013-08-23T120000.txt
2013-08-30T120000.txt"]
        ["cat_2.db"
         "Test cat 2
2013-08-23T120000.txt"]]))

(deftest post-cats-test
  (testing "post-cats test"
    (is (= (list (first categories))
           (post-cats "2013-08-30T120000.txt" categories)))))

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

(def month-test-data
  ["2013-08-23T120000.txt"
   "2013-08-30T120000.txt"
   "2013-09-20T120000.txt"
   "2013-07-03T120000.txt"])

(deftest sorted-months-test2
  (testing "Ordering of sorted-months"
    (is (= '(["2013" "09"] ["2013" "08"] ["2013" "07"])
           (map first (sorted-months month-test-data))))))

(deftest posts-month-ids-test
  (testing "post-month-ids"
    (is (= #{["2013" "09"] ["2013" "08"] ["2013" "07"]}
           (posts-month-ids month-test-data)))))

(deftest sorted-months-subset-test
  (testing "sorted-months-subset"
    (is (= ["2013" "08"]
           (ffirst
            (sorted-months-subset ["2013-08-23T120000.txt"]
                                  (sorted-months month-test-data)))))))

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
             :file "cat_10.db"
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
    (is (= (second categories)
           (binding [*cats* categories]
                    (find-cat-by-id "2"))))))

(deftest find-cat-by-id-num-test
  (testing "find-cat-by-id numeric"
    (is (= (second categories)
           (binding [*cats* categories]
                    (find-cat-by-id 2))))))

(deftest post-id-by-num-test
  (testing "post-id-by-num"
    (is (= "2013-08-28T120000.txt"
           (post-id-by-num '("2013-08-23T120000.txt"
                             "2013-08-28T120000.txt"
                             "2013-08-30T120000.txt")
                           2)))))

(deftest add-post-to-cat-simple
  (testing "add-post-to-cat simple case"
    (is (= (parse-cat "cat_1.db"
                      "Test cat 1
2013-08-23T120000.txt
2013-08-28T120000.txt
2013-08-30T120000.txt")
           (add-post-to-cat "2013-08-28T120000.txt"
                            (first categories))))))

(deftest add-post-to-cat-existent
  (testing "add-post-to-cat add existent post"
    (is (= (parse-cat "cat_1.db"
                      "Test cat 1
2013-08-23T120000.txt
2013-08-30T120000.txt")
           (add-post-to-cat "2013-08-23T120000.txt"
                            (first categories))))))

(deftest del-posts-from-cat-simple
  (testing "del-post-to-cat simple case"
    (is (= (parse-cat "cat_1.db"
                      "Test cat 1
2013-08-30T120000.txt")
           (del-posts-from-cat #{"2013-08-23T120000.txt"}
                               (first categories))))))


(deftest del-posts-from-cat-nonexist
  (testing "del-post-to-cat: removing nonexistant post"
    (is (= (parse-cat "cat_1.db"
                      "Test cat 1
2013-08-23T120000.txt
2013-08-30T120000.txt")
           (del-posts-from-cat #{"2013-08-28T120000.txt"}
                               (first categories))))))

(deftest category-next-id--empty
  (testing "category-next-id when there are no categories"
    (is (= "1" (category-next-id [])))))

(deftest category-next-id--standard
  (testing "category-next-id when there are number of categories"
    (is (= "3" (category-next-id categories)))))

(deftest command-number--empty
  (testing "command-number when no option provided"
    (is (= 0 (command-number {:add false})))))

(deftest command-number--add-only
  (testing "command-number when only --add is provided"
    (is (= 1 (command-number {:add true})))))

(deftest command-number--delete-only
  (testing "command-number when only --delete is provided"
    (is (= 1 (command-number {:add false :delete "1"})))))

(deftest command-number--add+del
  (testing "command-number when both --add and --delete are provided"
    (is (= 2 (command-number {:add true :delete "1"})))))

(deftest command-number--move+del
  (testing "command-number when both --move and --delete are provided"
    (is (= 2 (command-number {:add false :delete "1" :move "2"})))))
