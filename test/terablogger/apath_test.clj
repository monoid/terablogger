(ns terablogger.apath-test
  (:import java.io.File)
  (:use terablogger.cfg)
  (:require [clojure.test :refer :all]
            [terablogger.apath :refer :all]
            [clojure.string :as string]))

(defn p [& elts]
  "Join elements of path with File/separator."
  (string/join File/separator elts))

(def test-config
  "This is not a full config, but enough for testing."
  {:blog-dir (p "." "blog")
   :url ""})

(deftest url-path-test1
  (testing "url-path basic functionality"
    (is (= "mary/had/a/little/lamb"
           (url-path ["mary" "had" "a" "little" "lamb"])))))

(deftest blog-path-test1
  (testing "blog-path test"
    (is (= (p "." "blog" "test.html"))
        (with-config test-config
          (blog-path ["test.html"])))))

(deftest blog-path-test2
  (testing "blog-path test"
    (is (= (p "." "blog" "archive" "test.html")
           (with-config test-config
             (blog-path ["archive" "test.html"]))))))

(deftest data-path-test1
  (testing "data-path test"
    (is (= (p "." "blog" "data" "test.html")
           (with-config test-config
             (data-path "test.html"))))))

(deftest archive-apath-test1
  (testing "archive-apath test"
    (is (= ["archive" "test.html"]
           (archive-apath ["test.html"])))))

(deftest archive-path-test1
  (testing "archive-path test"
    (is (= (p "." "blog" "archive" "test.html")
           (with-config test-config
             (archive-path ["test.html"]))))))

(deftest archive-path-test2
  (testing "archive-path test"
    (is (= (p "." "blog" "archive" "cat_1" "test.html")
           (with-config test-config
             (archive-path ["cat_1" "test.html"]))))))

(deftest cache-path-test1
  (testing "archive-path test"
    (is (= (p "." "blog" "parts" "test.html")
           (with-config test-config
             (cache-path ["test.html"]))))))

