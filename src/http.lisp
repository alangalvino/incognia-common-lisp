(in-package :cl-incognia)

(defmacro do-request (&key uri method body basic-auth headers (parse-response t))
  `(let* ((response (dex:request ,uri
                                 :method ,method
                                 :basic-auth ,basic-auth
                                 :headers ,headers
                                 :content ,body)))
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
