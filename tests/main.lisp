(defpackage incognia-wrapper/tests/main
  (:use :cl
        :incognia-wrapper
        :rove))
(in-package :incognia-wrapper/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :incognia-wrapper)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
