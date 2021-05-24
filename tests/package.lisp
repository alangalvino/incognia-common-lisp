(defpackage cl-incognia
  (:use :cl)
  (:nicknames :incognia)
  (:export
           ;; For test only
           :plist-remove-null-values

           ;; Public functions
           :configure
           :make-address
           :send-feedback
           :register-signup
           :get-signup-assessment
           :register-payment
           :register-login))
