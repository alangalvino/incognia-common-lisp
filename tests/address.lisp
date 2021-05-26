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
      (ok (addr-coordinates-validp addr))))
  (testing "should be false if lat is not present"
    (let ((addr (make-address :lng 0.3)))
      (ng (addr-coordinates-validp addr))))
  (testing "should be false if lng is not present"
    (let ((addr (make-address :lat 0.3)))
      (ng (addr-coordinates-validp addr))))
  (testing "should be false if lng and lat are not present"
    (let ((addr (make-address)))
      (ng (addr-coordinates-validp addr)))))

(deftest test-addr-structured-validp
  (testing "should return T when any structured attribute"
    (let ((addr (make-address :locale "BR")))
      (ok (addr-structured-validp addr))))
  (testing "should return nil without structured attribute"
    (let ((addr (make-address)))
      (ng (addr-structured-validp addr)))))

(deftest test-addr-plist
    (testing "should return a valid plist"
             (let* ((addr (make-address
                           :type :|home|
                           :line "233 a street, CA, Berkeley, a bourough, a neighborhood, apt 2201, United States, 500100"
                           :lat 0.2
                           :lng 0.3
                           :locale "en-US"
                           :country-name "United States"
                           :country-code "US"
                           :state "CA"
                           :city "Berkeley"
                           :borough "a borough"
                           :neighborhood "a neighborhood"
                           :number "233"
                           :complements "apt 2201"
                           :postal-code "500100"
                           :street "a street"))
                    (addr-plist-obj (addr-plist addr)))
               (ok (equal (getf addr-plist-obj :|type|) :|home|))
               (ok (equal (getf addr-plist-obj :|address_line|) "233 a street, CA, Berkeley, a bourough, a neighborhood, apt 2201, United States, 500100"))

               (ok (equal (getf addr-plist-obj :|lat|) 0.2))
               (ok (equal (getf addr-plist-obj :|lng|) 0.3))
               (ok (equal (getf addr-plist-obj :|locale|) "en-US"))
               (ok (equal (getf addr-plist-obj :|country_code|) "US"))
               (ok (equal (getf addr-plist-obj :|country_name|) "United States"))
               (ok (equal (getf addr-plist-obj :|state|) "CA"))
               (ok (equal (getf addr-plist-obj :|city|) "Berkeley"))
               (ok (equal (getf addr-plist-obj :|borough|) "a borough"))
               (ok (equal (getf addr-plist-obj :|neighborhood|) "a neighborhood"))
               (ok (equal (getf addr-plist-obj :|number|) "233"))
               (ok (equal (getf addr-plist-obj :|complements|) "apt 2201"))
               (ok (equal (getf addr-plist-obj :|postal_code|) "500100"))
               (ok (equal (getf addr-plist-obj :|street|) "a street")))))
