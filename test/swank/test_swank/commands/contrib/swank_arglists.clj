(ns swank.test-swank.commands.contrib.swank-arglists
  (:refer-clojure :exclude [load-file])
  (:use swank.commands.contrib.swank-arglists :reload-all)
  (:use clojure.test))

(defn emacs-package-fixture [f]
  (binding [swank.core/*current-package* "user"]
    (f)))

(use-fixtures :each emacs-package-fixture)

(deftest autodoc-test
  (testing "arglist for symbol"
    (is (= (str ((meta #'list) :arglists)) (autodoc* '( "list" :cursor-marker)))))
  (testing "arglist for symbol with args"
    (is (= (str ((meta #'list) :arglists)) (autodoc* '( "list" "a" :b :cursor-marker)))))
  (testing "arglist for nothing"
    (is (= ':not-available (autodoc* '( :cursor-marker)))))
  (testing "arglist for unreadable symbol"
    (is (= ':not-available (autodoc* '("clojure.")))))
  (testing "arglist for undefined symbol"
    (is (= ':not-available (autodoc* '("abcde123"))))))
