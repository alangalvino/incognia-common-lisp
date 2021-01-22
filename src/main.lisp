(defpackage incognia-wrapper
  (:use :cl)
  (:nicknames :incognia-apis)
  (:export :authenticate
           :feedbacks
           :onboarding-signups))
(in-package :incognia-wrapper)

(defvar *incognia-uri* "https://incognia.inloco.com.br/")

(defvar *authentication-uri* (concatenate 'string *incognia-uri* "api/v1/token"))

(defvar *onboarding-signups-uri* (concatenate 'string *incognia-uri* "api/v2/onboarding/signups"))

(defvar *feedbacks-uri* (concatenate 'string *incognia-uri* "api/v2/feedbacks"))

(defvar *auth-token* nil)

(defun print-hash-key-value-with-tab (tab key &optional value (stream t))
  ;; ex. for tab equals 1 "~%1@t~a: ~a:"
  (format stream "~@[~%~v@t~a: ~] ~@[~a~]" tab key value))

(defun prettyprint-hash-table (hash-table &optional (tab 0))
  (loop for key being the hash-keys of hash-table
          using (hash-value value)
        do (if (equal (type-of value) 'hash-table)
               (progn
                 (print-hash-key-value-with-tab tab key)
                 (prettyprint-hash-table value (+ tab 1)))
               (print-hash-key-value-with-tab tab key value))))

(defun parse-yaml-file ()
  (yaml:parse (asdf:system-relative-pathname :incognia-wrapper #p"./credentials.yaml")))

(defun credentials-from-yaml ()
  (let* ((yaml-file (parse-yaml-file)))
    (cons (gethash "client-id" yaml-file) (gethash "client-secret" yaml-file))))

(defun get-access-token (token-response)
  (getf (jonathan:parse token-response)
        :|access_token|))

(defun revoke-auth-token ()
  (setf *auth-token* nil))

(defun authenticate (&optional credentials-cons)
  (setf *auth-token*
        (let* ((credentials (or credentials-cons (credentials-from-yaml))))
          (get-access-token (dexador:post *authentication-uri*
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
  (prettyprint-hash-table
   (jonathan:parse (dexador:post *onboarding-signups-uri*
                                 :headers (list
                                           '("Content-Type" . "application/json")
                                           (cons "Authorization" (concatenate 'string "Bearer " *auth-token*)))
                                 :content (jonathan:to-json (list :|installation_id| installation-id
                                                                  :|address_line| address-line
                                                                  :|app_id| app-id)))
                   :as :hash-table)))

;; Example
#+nil
(authenticate)

#+nil
(incognia-apis:onboarding-signups :installation-id "installation-id"
                                  :address-line "address-line")
