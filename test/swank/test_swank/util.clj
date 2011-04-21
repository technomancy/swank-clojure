(ns swank.test-swank.util
  (:use swank.util
        clojure.test))

(deftest test-one-of?
  (testing "matches"
    (is (one-of? :a :a :b (throw (Exception. "Failed to short circuit"))))
    (is (one-of? 1 1))
    (is (one-of? "one" "one" "two" "three")))
  (testing "mismatches"
    (is (not (one-of? :a :b :c :d)))
    (is (not (one-of? 1 2)))
    (is (not (one-of? "one" "two" "three")))
    (is (thrown-with-msg? Exception #"None found"
          (one-of? :a :b :c (throw (Exception. "None found")))))))

(deftest test-find-first
  (testing "first true"
    (are [coll first-true] (= (find-first coll) first-true)
         [1 2 3] 1
         [nil false :first] :first))
  (testing "with predicate"
    (are [coll pred first-true] (= (find-first pred coll) first-true)
         [1 2 3 4 5] even?  2
         [1 2 3 4 5] #{3 4} 3))
  (testing "non existent"
    (are [coll pred] (nil? (find-first pred coll))
         [1 3 5 7 9] even?
         [1 2 3 4 5] #{6 7})))

(deftest test-position
  (testing "with matches"
    (are [coll pred pos] (= (position pred coll) pos)
         [:a :b :c :d] #{:c} 2))
  (testing "with matches and starting position"
    (are [coll pred start pos] (= (position pred coll start) pos)
         [:a :b :a :b :a :b] #{:a} 1 2))
  (testing "without matches"
    (are [coll pred] (not (position pred coll))
         [1 3 5 7] even?
         [:a :b :c :d] #{:e})))
