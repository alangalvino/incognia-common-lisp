(defpackage cl-incognia/tests/auth
  (:use :cl
        :cl-incognia
        :rove))
(in-package :cl-incognia/tests/auth)

(deftest test-auth-token-validp
  (testing "should return nil when token is invalid"
    (setf *auth-token* (list :|access_token| nil
                             :|expires_in| "200"
                             :|created_at| (- (get-universal-time) 100)))
    (ng (auth-token-validp) nil))
  (testing "should return nil when token is expired"
    (setf *auth-token* (list :|access_token| "@cesst0ken"
                             :|expires_in| "200"
                             :|created_at| (- (get-universal-time) 400)))
    (ng (auth-token-validp) nil))
  (testing "should return T when token is valid"
    (setf *auth-token* (list :|access_token| "@cesst0ken"
                             :|expires_in| "200"
                             :|created_at| (- (get-universal-time) 100)))
    (ok (auth-token-validp))))
