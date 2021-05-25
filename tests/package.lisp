(defpackage cl-incognia
  (:use :cl)
  (:nicknames :incognia)
  (:export
           ;; Utils methods for test only
           :plist-remove-null-values

           ;; Address methods for test only
           :addr-coordinates-validp
           :addr-line-validp

           ;; Public functions
           :configure
           :make-address
           :send-feedback
           :register-signup
           :get-signup-assessment
           :register-payment
           :register-login))
