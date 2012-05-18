(ns swank.test-swank.util.class-browse
  (:use swank.util.class-browse
        clojure.test)
  (:import [java.io File]))

(def test-jar (File. "test/data/test.jar"))

(deftest test-path-class-files-jar
  (testing "class from jar file"
    (is (= (:name (first (path-class-files test-jar test-jar))) "a.b.c.d.Test"))))
