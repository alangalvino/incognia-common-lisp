(defpackage incognia-wrapper/tests/util
  (:use :cl
        :incognia-wrapper.util
        :rove))
(in-package :incognia-wrapper/tests/util)

;; NOTE: To run this test file, execute `(asdf:test-system :incognia-wrapper)' in your Lisp.

(deftest test-plist-remove-null-value
  (testing "should remove null values of a simple property list"
    (let* ((plist '(:one-key 1 :other-key nil)))
      (ok (equal
           (incognia-wrapper.util:plist-remove-null-values plist)
           '(:one-key 1)))))
  (testing "should remove null values of a simple property list"
    (let* ((plist '(:one-key 1 :other-key nil)))
      (ok (equal
           (incognia-wrapper.util:plist-remove-null-values plist)
           '(:one-key 1))))))
