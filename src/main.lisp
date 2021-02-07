o(defpackage incognia-wrapper
  (:use :cl)
  (:nicknames :incognia-apis)
  (:export :authenticate
           :feedbacks
           :onboarding-signups))
(in-package :incognia-wrapper)

;; Incognia APIs URIs
(defvar *incognia-uri* "https://incognia.inloco.com.br/")
(defvar *authentication-uri* (concatenate 'string *incognia-uri* "api/v1/token"))
(defvar *onboarding-signups-uri* (concatenate 'string *incognia-uri* "api/v2/onboarding/signups"))
(defvar *feedbacks-uri* (concatenate 'string *incognia-uri* "api/v2/feedbacks"))

(defvar *auth-token* nil)

(defun revoke-auth-token ()
  (setf *auth-token* nil))

(defun parse-access-token (token-response)
  (getf (jonathan:parse token-response) :|access_token|))

(defun authenticate (&optional credentials-cons)
  (setf *auth-token*
        (let* ((credentials (or credentials-cons (incognia-apis.config:load-credentials-from-yaml))))
          (parse-access-token (dexador:post *authentication-uri*
                                            :basic-auth credentials
                                            :headers '(("Content-Type" . "application/x-www-form-urlencoded")))))))

(defun feedbacks (&key timestamp event app-id (installation-id "") (account-id "") (signup-id ""))
  (dexador:post *feedbacks-uri*
                :headers (list
                          '("Content-Type" . "application/json")
                          (cons "Authorization" (concatenate 'string "Bearer " *auth-token*)))
                :content (jonathan:to-json (list :|timestamp| timestamp
                                                 :|event| event
                                                 :|app_id| app-id
                                                 :|installation_id| installation-id
                                                 :|account_id| account-id
                                                 :|signup_id| signup-id))))

(defun onboarding-signups (&key installation-id address-line app-id)
  (dexador:post *onboarding-signups-uri*
                :headers (list
                          '("Content-Type" . "application/json")
                          (cons "Authorization" (concatenate 'string "Bearer " *auth-token*)))
                :content (jonathan:to-json (list :|installation_id| installation-id
                                                 :|address_line| address-line
                                                 :|app_id| app-id))))

;; Example
#+nil
(authenticate)

#+nil
(incognia-apis:onboarding-signups :installation-id "installation-id"
                                  :address-line "address-line")
