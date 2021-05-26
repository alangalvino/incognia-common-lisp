(defpackage cl-incognia/tests/config
  (:use :cl
        :cl-incognia
        :rove))
(in-package :cl-incognia/tests/config)

(deftest test-configure
  (testing "should create a config with us region as default"
    (incognia:configure :client-id "client-id"
                        :client-secret "client-secret")
    (ok (getf *api-config* :region) :us)))

(deftest test-incognia-uri
  (testing "should return US region url"
    (incognia:configure :client-id "client-id"
                        :client-secret "client-secret")
    (ok (equal (incognia-uri "test") (concatenate 'string "https://api.us.incognia.com/" "test"))))
  (testing "should return US region url"
    (incognia:configure :client-id "client-id"
                        :client-secret "client-secret"
                        :region :us)
    (ok (equal (incognia-uri "test") (concatenate 'string "https://api.us.incognia.com/" "test"))))
  (testing "should return BR region url"
    (incognia:configure :client-id "client-id"
                        :client-secret "client-secret"
                        :region :br)
    (ok (equal (incognia-uri "test") (concatenate 'string "https://incognia.inloco.com.br/" "test")))))
