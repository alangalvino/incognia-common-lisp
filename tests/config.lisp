(defpackage incognia-wrapper/tests/config
  (:use :cl
        :incognia-wrapper.config
        :rove))
(in-package :incognia-wrapper/tests/config)

;; NOTE: To run this test file, execute `(asdf:test-system :incognia-wrapper)' in your Lisp.

(deftest test-load-credentials-from-file
  (testing "should load the credentials from file"
    (let* ((credentials (incognia-wrapper.config:load-credentials-from-yaml #p"tests/credentials.yaml")))
      (ok (equal credentials '("client-id-01" . "client-secret-01"))))))

(run-suite *package*)
