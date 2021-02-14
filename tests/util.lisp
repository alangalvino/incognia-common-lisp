(defpackage incognia-wrapper/tests/util
  (:use :cl
        :incognia-wrapper.util
        :rove))
(in-package :incognia-wrapper/tests/util)

;; NOTE: To run this test file, execute `(asdf:test-system :incognia-wrapper)' in your Lisp.

(deftest test-plist-remove-null-value
  (testing "should remove null values of a simple property list"
    (let* ((plist '(:key-1 1 :key-2 nil :key-3 3)))
      (ok (equal
           (incognia-wrapper.util:plist-remove-null-values plist)
           '(:key-1 1 :key-3 3)))))
  (testing "should remove null values of a nested (1 layer) property list"
    (let* ((plist '(:key-1 1 :key-2 (:key-3 nil) :key-4 4)))
      (ok (equal
           (incognia-wrapper.util:plist-remove-null-values plist)
           '(:key-1 1 :key-4 4)))))
  (testing "should remove null values of a nested (2 layers) property list"
    (let* ((plist '(:key-1 1 :key-2 (:key-3 (:key-4 nil)) :key-5 5)))
      (ok (equal
           (incognia-wrapper.util:plist-remove-null-values plist)
           '(:key-1 1 :key-5 5)))))
  (testing "should return an empty list"
    (let* ((plist '(:key-1 nil)))
      (ok (equal
           (incognia-wrapper.util:plist-remove-null-values plist)
           '()))))
  (testing "should return an empty list"
    (let* ((plist '()))
      (ok (equal
           (incognia-wrapper.util:plist-remove-null-values plist)
           '())))))
