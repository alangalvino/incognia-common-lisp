(defpackage cl-incognia
  (:use :cl)
  (:nicknames :incognia)
  (:export
           ;; Utils methods for test only
           :plist-remove-null-values

           ;; Address methods for test only
           :addr-coordinates-validp
           :addr-line-validp
           :addr-structured-validp
           :addr-plist

           ;; Config methods for teste only
           *api-config*
           incognia-uri

           ;; Auth methods for test only
           *auth-token*
           :auth-token-validp

           ;; Public functions
           :configure
           :make-address
           :send-feedback
           :register-signup
           :get-signup-assessment
           :register-payment
           :register-login))
