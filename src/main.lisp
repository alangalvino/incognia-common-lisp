(defpackage incognia-wrapper
  (:use :cl)
  (:nicknames :incognia)
  (:export :configure
           :feedbacks
           :signups
           :transactions))
(in-package :incognia-wrapper)

;; Incognia APIs URIs
(defvar *incognia-uri* "https://incognia.inloco.com.br/")
(defvar *authentication-uri* (concatenate 'string *incognia-uri* "api/v1/token"))
(defvar *signups-uri* (concatenate 'string *incognia-uri* "api/v2/onboarding/signups"))
(defvar *transactions-uri* (concatenate 'string *incognia-uri* "api/v2/authentication/transactions"))
(defvar *feedbacks-uri* (concatenate 'string *incognia-uri* "api/v2/feedbacks"))

(defvar *auth-token* nil)
(defvar *api-config* ())

(defun to-json (plist)
  (jonathan:to-json (incognia.util:plist-remove-null-values plist)))

(defun parse-json (alist)
  (jonathan:parse alist))

(defun token-valid-p ()
  (let ((expires-in (parse-integer (getf *auth-token* :|expires_in|)))
        (created-at (getf *auth-token* :|created_at|))
        (now (get-universal-time)))
    (and *auth-token* (> expires-in (- now created-at)))))

(defun auth-token ()
  (if (and *auth-token* (token-valid-p))
      *auth-token*
      (update-token)))

(defun credentials ()
  (let ((client-id (getf *api-config* :client-id))
        (client-secret (getf *api-config* :client-secret)))
    (cons client-id client-secret)))

(defun configure (&key client-id client-secret region)
  (progn (if client-id (setf (getf *api-config* :client-id) client-id))
         (if client-secret (setf (getf *api-config* :client-secret) client-secret))
         (if region (setf (getf *api-config* :region) region))))

(defun update-token ()
  (progn (setf *auth-token* (authenticate))
         (setf (getf *auth-token* :|created_at|) (get-universal-time))
         *auth-token*))

(defmacro do-request (&key uri method body (basic-auth nil) headers (parse-response t))
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

(defun authenticate ()
  (do-request
    :uri *authentication-uri*
    :method :post
    :basic-auth (credentials)
    :headers '(("Content-Type" . "application/x-www-form-urlencoded"))))

(defun feedbacks (&key timestamp event app-id installation-id account-id signup-id login-id transaction-id)
  (do-request
    :uri *feedbacks-uri*
    :method :post
    :body (to-json (list :|timestamp| timestamp
                         :|event| event
                         :|app_id| app-id
                         :|installation_id| installation-id
                         :|login_id| login-id
                         :|transaction_id| transaction-id
                         :|account_id| account-id
                         :|signup_id| signup-id))))

(defun signups (&key installation-id address-line app-id)
  (do-auth-request
    :uri *signups-uri*
    :method :post
    :body (to-json (list :|installation_id| installation-id
                         :|address_line| address-line
                         :|app_id| app-id))))

(defun transactions (&key installation-id account-id type app-id)
  (do-request
    :uri *transactions-uri*
    :method :post
    :body (to-json (list :|installation_id| installation-id
                         :|account_id| account-id
                         :|type| type
                         :|app_id| app-id))))

;; Example
#+nil
(authenticate)

#+nil
(incognia:signups :installation-id "installation-id"
                  :address-line "address-line")
