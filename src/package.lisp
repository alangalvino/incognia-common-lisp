(defpackage cl-incognia
  (:use :cl)
  (:nicknames :incognia)
  (:export :configure
           :make-address
           :send-feedback
           :register-signup
           :get-signup-assessment
           :register-payment
           :register-login

           ;; http request error
           :http-request-error
           :response-body
           :response-status
           :request-uri
           :request-method))
