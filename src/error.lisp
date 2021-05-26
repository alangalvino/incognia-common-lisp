(in-package :cl-incognia)

(define-condition http-request-error (error)
  ((response-status :initarg :response-status
                    :initform nil
                    :accessor response-status)
   (response-body :initarg :response-body
                  :initform nil
                  :accessor response-body)
   (request-uri :initarg :request-uri
                :initform nil
                :accessor request-uri)
   (request-method :initarg :request-method
                   :initform nil
                   :accessor request-method))
  ;; the :report is the message into the debugger:
  (:report (lambda (condition stream)
             (let ((response-status (response-status condition))
                   (request-method (request-method condition))
                   (request-uri (request-uri condition))
                   (response-body (response-body condition)))
               (format stream "[http-request-error] response-code: ~D, request-method: ~D, request-uri: ~D, response-body: ~D" response-status request-method request-uri response-body)))))
