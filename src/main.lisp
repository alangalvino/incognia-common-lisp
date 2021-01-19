(defpackage incognia-wrapper
  (:use :cl)
  (:nicknames :incognia-apis)
  (:export :onboarding-signups))
(in-package :incognia-wrapper)

(defvar *incognia-uri* "https://incognia.inloco.com.br/")

(defvar *authentication-uri* (concatenate 'string *incognia-uri* "api/v1/token"))

(defvar *onboarding-signups-uri* (concatenate 'string *incognia-uri* "api/v2/onboarding/signups"))

(defvar *auth-token* nil)

(defun print-hash-key-with-tab (key tab-number)
  ;; ex. for tab-number equals 1 "~%1@t~a:"
  (format t (format nil "~~%~~~a@t~~a:" tab-number) key))

(defun print-hash-key-value-with-tab (key value tab-number)
  ;; ex. for tab-number equals 1 "~%1@t~a: ~a:"
  (format t (format nil "~~%~~~a@t~~a: ~~a" tab-number) key value))

(defun prettyprint-hash-table (hash-table &optional (tab-number 0))
  (loop for key being the hash-keys of hash-table
          using (hash-value value)
        do (if (equal (type-of value) 'hash-table)
               (progn
                 (print-hash-key-with-tab key tab-number)
                 (prettyprint-hash-table value (+ tab-number 1)))
               (print-hash-key-value-with-tab key value tab-number))))

(defun parse-yaml-file ()
  (yaml:parse (asdf:system-relative-pathname :incognia-wrapper #p"./credentials.yaml")))

(defun credentials-from-yaml ()
  (let* ((yaml-file (parse-yaml-file)))
    (cons (gethash "client-id" yaml-file) (gethash "secret" yaml-file))))

(defun get-access-token (token-response)
  (getf (jonathan:parse token-response)
        :|access_token|))

(defun revoke-auth-token ()
  (setf *auth-token* nil))

(defun authenticate (&optional credentials-arg)
  (setf *auth-token*
        (let* ((credentials (or credentials-arg (credentials-from-yaml))))
          (get-access-token (dexador:post *authentication-uri*
                                          :basic-auth credentials
                                          :headers '(("Content-Type" . "application/x-www-form-urlencoded")))))))

(defun onboarding-signups-request-body (installation-id address-line &optional app-id)
  (jonathan:to-json (list :|installation_id| installation-id :|address_line| address-line :|app_id| app-id)))

;; TODO: add GET method
(defun onboarding-signups (&key installation-id address-line app-id credentials)
  (let* ((token (or *auth-token* (authenticate credentials))))
    (prettyprint-hash-table (jonathan:parse (dexador:post *onboarding-signups-uri*
                                                          :headers (list
                                                                    '("Content-Type" . "application/json")
                                                                    (cons "Authorization" (concatenate 'string "Bearer " token)))
                                                          :content (onboarding-signups-request-body installation-id address-line app-id))
                                            :as :hash-table))))

;; Example
#+nil
(incognia-apis:onboarding-signups :installation-id "installation-id"
                                  :address-line "address-line")
