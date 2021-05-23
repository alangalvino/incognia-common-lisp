(defpackage incognia-wrapper
  (:use :cl)
  (:nicknames :incognia)
  (:export :configure
           :send-feedback
           :register-signup
           :get-signup-assessment
           :register-payment
           :register-login))
(in-package :incognia-wrapper)

;; Incognia APIs URIs
(defvar *incognia-br-uri* "https://incognia.inloco.com.br/")
(defvar *incognia-us-uri* "https://api.us.incognia.com/")

;; Incognia APIs Resource URIs
(defvar *authentication-uri* "api/v1/token/")
(defvar *signups-uri* "api/v2/onboarding/signups/")
(defvar *transactions-uri* "api/v2/authentication/transactions/")
(defvar *feedbacks-uri* "api/v2/feedbacks/")

(defvar *auth-token* nil)
(defvar *api-config* ())

(deftype feedback-event-type () '(member :|signup_accepted| :|signup_declined| :|payment_accepted| :|payment_declined| :|payment_declined_by_risk_analysis| :|payment_declined_by_manual_review| :|payment_declined_by_business| :|payment_declined_by_acquirer| :|login_accepted| :|login_declined| :|verified| :|not_verified| :|account_takeover| :|chargeback|))
(deftype region-type () '(member :br :us))

(defun us-region-p ()
  (eq (getf *api-config* :region) :us))

(defun revoke-token ()
  (setf *auth-token* nil))

(defun incognia-uri (resource-uri)
  (concatenate 'string (if (us-region-p) *incognia-us-uri*
                           *incognia-br-uri*) resource-uri))

(defun to-json (plist)
  (jonathan:to-json (incognia.util:plist-remove-null-values plist)))

(defun parse-json (alist)
  (jonathan:parse alist))

(defun auth-token-valid-p ()
  (let ((expires-in (parse-integer (getf *auth-token* :|expires_in|)))
        (created-at (getf *auth-token* :|created_at|))
        (now (get-universal-time)))
    (and *auth-token* (> expires-in (- now created-at)))))

(defun auth-token ()
  (if (and *auth-token* (getf *auth-token* :|access_token|) (auth-token-valid-p))
      *auth-token*
      (update-token)))

(defun credentials ()
  (let ((client-id (getf *api-config* :client-id))
        (client-secret (getf *api-config* :client-secret)))
    (cons client-id client-secret)))

(defun configure (&key client-id client-secret region)
  (check-type region region-type)
  (if client-id (setf (getf *api-config* :client-id) client-id))
  (if client-secret (setf (getf *api-config* :client-secret) client-secret))
  (if region (setf (getf *api-config* :region) region))
  (revoke-token))

(defun update-token ()
  (setf *auth-token* (authenticate))
  (setf (getf *auth-token* :|created_at|) (get-universal-time))
  *auth-token*)

(defmacro do-request (&key uri method body basic-auth headers (parse-response t))
  `(let* ((response (handler-case (dex:request ,uri
                                               :method ,method
                                               :basic-auth ,basic-auth
                                               :headers ,headers
                                               :content ,body)
                      (dex:http-request-failed (e)
                        (format nil "{ \"error\": \"http request to ~d has failed with status code ~D and body ~d\"}" (quri:render-uri (dex:request-uri e)) (dex:response-status e) (dex:response-body e))))))
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
    :uri (incognia-uri *authentication-uri*)
    :method :post
    :basic-auth (credentials)
    :headers '(("Content-Type" . "application/x-www-form-urlencoded"))))

(defun send-feedback (&key timestamp event installation-id account-id)
  (check-type event feedback-event-type)
  (do-auth-request
    :uri (incognia-uri *feedbacks-uri*)
    :method :post
    :body (to-json (list :|timestamp| timestamp
                         :|event| event
                         :|installation_id| installation-id
                         :|account_id| account-id))))

(defun get-signup-assessment (&key signup-id)
  (do-auth-request
    :uri (concatenate 'string  (incognia-uri *signups-uri*) signup-id)
    :method :get))

(defun register-signup (&key installation-id address-line app-id)
  (do-auth-request
    :uri (incognia-uri *signups-uri*)
    :method :post
    :body (to-json (list :|installation_id| installation-id
                         :|address_line| address-line
                         :|app_id| app-id))))

(defun register-transaction (&key installation-id account-id type app-id addresses)
  (do-auth-request
    :uri (incognia-uri *transactions-uri*)
    :method :post
    :body (to-json `(:|installation_id| ,installation-id
                     :|account_id| ,account-id
                     :|type| ,type
                     ,@(if addresses '(:|addresses| addresses))
                     ,@(if app-id '(:|app_id| app-id))))))

(defun register-login (&key installation-id account-id app-id)
  (register-transaction :installation-id installation-id
                        :account-id account-id
                        :type :|login|
                        :app-id app-id))

(defun register-payment (&key installation-id account-id app-id addresses)
  (register-transaction :installation-id installation-id
                        :account-id account-id
                        :addresses addresses
                        :type :|payment|
                        :app-id app-id))
