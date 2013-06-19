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
   :url "http://test/blog/"})

(deftest url-path-test1
  (testing "url-path basic functionality"
    (is (= "mary/had/a/little/lamb"
           (url-path ["mary" "had" "a" "little" "lamb"])))))

(deftest full-url-path-test1
  (testing "full-url-path function"
    (is (= "http://test/blog/mary/had/a/little/lamb"
           (with-config test-config
             (full-url-path ["mary" "had" "a" "little" "lamb"]))))))

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
           (archive ["test.html"])))))

(deftest cache-apath-test1
  (testing "archive-path test"
    (is (= ["parts" "test.html"]
           (with-config test-config
             (cache ["test.html"]))))))

