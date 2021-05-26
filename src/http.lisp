(in-package :cl-incognia)

(defmacro do-request (&key uri method body basic-auth headers (parse-response t))
  `(let* ((response (handler-case (dex:request ,uri
                                               :method ,method
                                               :basic-auth ,basic-auth
                                               :headers ,headers
                                               :content ,body)
                      (t (e)
                        (let ((response-status (dex:response-status e))
                              (response-body (dex:response-body e))
                              (request-method (dex:request-method e))
                              (request-uri (quri:render-uri (dex:request-uri e))))
                          (error 'http-request-error :response-status response-status
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
