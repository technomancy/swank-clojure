(ns swank.test-swank.commands.basic
  (:refer-clojure :exclude [load-file print-doc])
  (:use swank.commands.basic :reload-all)
  (:use clojure.test))

(defn emacs-package-fixture [f]
  (binding [swank.core/*current-package* "user"]
    (f)))

(use-fixtures :each emacs-package-fixture)

(defmacro with-private-vars [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context.  From users mailing
list, Alan Dipert and MeikelBrandmeyer."
  `(let ~(reduce #(conj %1 %2 `@(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(with-private-vars [swank.commands.basic
                    [guess-compiler-exception-location
                     exception-location]]

  (deftest guess-compiler-exception-location-test
    (is (= '(:location (:file "a.clj") (:line 1) nil)
           (guess-compiler-exception-location
            (clojure.lang.Compiler$CompilerException. "a.clj" 1 (Exception. "err"))))))

  (deftest exception-location-test
    (is (= '(:location (:file "a.clj") (:line 1) nil)
           (exception-location
            (clojure.lang.Compiler$CompilerException. "a.clj" 1 (Exception. "err")))))))
