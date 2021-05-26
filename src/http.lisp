(in-package :cl-incognia)

(define-condition http-request-error (error)
  ((http-status :initarg :http-status
                :initform nil
                :accessor http-status)
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
             (let ((http-status (http-status condition))
                   (request-method (request-method condition))
                   (request-uri (request-uri condition))
                   (response-body (response-body condition)))
               (format stream "[http-request-error] status-code: ~D, request-method: ~D, request-uri: ~D, response-body: ~D" http-status request-method request-uri response-body)))))

(defmacro do-request (&key uri method body basic-auth headers (parse-response t))
  `(let* ((response (handler-case (dex:request ,uri
                                               :method ,method
                                               :basic-auth ,basic-auth
                                               :headers ,headers
                                               :content ,body)
                      (t (e)
                        (let ((http-status (dex:response-status e))
                              (response-body (dex:response-body e))
                              (request-method (dex:request-method e))
                              (request-uri (quri:render-uri (dex:request-uri e))))
                          (error 'http-request-error :http-status http-status
                                                     :response-body response-body
                                                     :request-method request-method
                                                     :request-uri request-uri))))))
     (if (and response ,parse-response)
         (parse-json response)
         response)))

(defmacro do-auth-request (&key uri method body)
  `(let* ((token (getf (auth-token) :|access_token|)))
     (do-request
       :uri ,uri
       :method ,method
       :headers (list
                 '("Content-Type" . "application/json")
                 (cons "Authorization" (concatenate 'string "Bearer " token)))
       :body ,body)))
