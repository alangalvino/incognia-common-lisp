(defpackage cl-incognia/tests/address
  (:use :cl
        :cl-incognia
        :rove))
(in-package :cl-incognia/tests/address)

(deftest test-address-creation
  (testing "should raise an error when adr-type is invalid"
    (ok (signals (make-address :type :invalid-type))))
  (testing "should create an address when type is valid"
    (let ((addr (make-address :type :|home|)))
      (ok addr))))

(deftest test-addr-line-validp
  (testing "should return T when :lat and :lng are float"
    (let ((addr (make-address :line "Hello World")))
      (ok (addr-line-validp addr))))
  (testing "should be false if lat is not present"
    (let ((addr (make-address)))
      (ng (addr-line-validp addr)))))

(deftest test-addr-coordinates-validp
  (testing "should return T when :lat and :lng are float"
    (let ((addr (make-address :lat 0.2 :lng 0.3)))
      (ok (and (addr-coordinates-validp addr)))))
  (testing "should be false if lat is not present"
    (let ((addr (make-address :lng 0.3)))
      (ng (addr-coordinates-validp addr))))
  (testing "should be false if lng is not present"
    (let ((addr (make-address :lat 0.3)))
      (ng (addr-coordinates-validp addr))))
  (testing "should be false if lng and lat are not present"
    (let ((addr (make-address)))
      (ng (addr-coordinates-validp addr)))))
