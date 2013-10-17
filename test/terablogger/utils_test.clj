(ns terablogger.utils-test
  (:import java.io.File)
  (:require [clojure.test :refer :all]
            [terablogger.utils :refer :all]
            [clojure.string :as string]))


(deftest sort*-default-test
  (testing "sort* with default comparator"
    (is (= [5 3 2 2 1]
           (sort* [1 2 5 3 2])))))

(deftest sort*-test
  (testing "sort* with comparator"
    (is (= [{:id 5} {:id 3} {:id 2} {:id 2} {:id 1}]
           (sort* #(compare (:id %1) (:id %2))
                  [{:id 1} {:id 2} {:id 5} {:id 3} {:id 2}])))))
