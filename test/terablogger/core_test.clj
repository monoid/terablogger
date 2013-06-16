(ns terablogger.core-test
  (:import java.io.File)
  (:require [clojure.test :refer :all]
            [terablogger.core :refer :all]
            [clojure.string :as string]))

(defn p [& elts]
  "Join elements of path with File/separator."
  (string/join File/separator elts))

(def test-config
  "This is not a full config, but enough for testing."
  {:blog-dir (p "." "blog")})


(deftest url-path-test1
  (testing "url-path basic functionality"
    (is (= "mary/had/a/little/lamb"
           (url-path ["mary" "had" "a" "little" "lamb"])))))

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


(deftest blog-path-test1
  (testing "blog-path test"
    (is (= (p "." "blog" "test.html"))
        (with-config test-config
          (blog-path "test.html")))))

(deftest blog-path-test2
  (testing "blog-path test"
    (is (= (p "." "blog" "archive" "test.html")
           (with-config test-config
             (blog-path "archive" "test.html"))))))

(deftest data-path-test1
  (testing "data-path test"
    (is (= (p "." "blog" "data" "test.html")
           (with-config test-config
             (data-path "test.html"))))))

(deftest archive-path-test1
  (testing "archive-path test"
    (is (= (p "." "blog" "archive" "test.html")
           (with-config test-config
             (archive-path "test.html"))))))

(deftest archive-path-test2
  (testing "archive-path test"
    (is (= (p "." "blog" "archive" "cat_1" "test.html")
           (with-config test-config
             (archive-path "cat_1" "test.html"))))))

(deftest cache-path-test1
  (testing "archive-path test"
    (is (= (p "." "blog" "parts" "test.html")
           (with-config test-config
             (cache-path "test.html"))))))

