(defpackage incognia-wrapper
  (:use :cl)
  (:nicknames :incognia)
  (:export :authenticate
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

(defun revoke-auth-token ()
  (setf *auth-token* nil))

(defun to-json (plist)
  (jonathan:to-json (incognia.util:plist-remove-null-values plist)))

(defun parse-json (alist)
  (jonathan:parse alist))

(defun parse-access-token (token-response)
  (getf (parse-json token-response) :|access_token|))

(defmacro do-request (&key uri method content)
  `(let* ((response (dex:request ,uri
                                 :method ,method
                                 :headers (list
                                           '("Content-Type" . "application/json")
                                           (cons "Authorization" (concatenate 'string "Bearer " *auth-token*)))
                                 :content ,content)))
     (if response (parse-json response))))

(defun authenticate (&optional credentials-cons)
  (setf *auth-token*
        (let* ((credentials (or credentials-cons (incognia.config:load-credentials-from-yaml))))
          (parse-access-token (dexador:post *authentication-uri*
                                            :basic-auth credentials
                                            :headers '(("Content-Type" . "application/x-www-form-urlencoded")))))))

(defun feedbacks (&key timestamp event app-id installation-id account-id signup-id)
  (do-request
    :uri *feedbacks-uri*
    :method :post
    :content (to-json (list :|timestamp| timestamp
                            :|event| event
                            :|app_id| app-id
                            :|installation_id| installation-id
                            :|account_id| account-id
                            :|signup_id| signup-id))))

(defun signups (&key installation-id address-line app-id)
  (do-request
    :uri *signups-uri*
    :method :post
    :content (to-json (list :|installation_id| installation-id
                            :|address_line| address-line
                            :|app_id| app-id))))

(defun transactions (&key installation-id account-id type app-id)
  (do-request
    :uri *transactions-uri*
    :method :post
    :content (to-json (list :|installation_id| installation-id
                            :|account_id| account-id
                            :|type| type
                            :|app_id| app-id))))

;; Example
#+nil
(authenticate)

#+nil
(incognia:signups :installation-id "installation-id"
                  :address-line "address-line")
