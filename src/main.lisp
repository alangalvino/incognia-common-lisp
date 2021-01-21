(defpackage incognia-wrapper
  (:use :cl)
  (:nicknames :incognia-apis)
  (:export :authenticate
           :onboarding-signups
           :prompt))
(in-package :incognia-wrapper)

(defvar *incognia-uri* "https://incognia.inloco.com.br/")

(defvar *authentication-uri* (concatenate 'string *incognia-uri* "api/v1/token"))

(defvar *onboarding-signups-uri* (concatenate 'string *incognia-uri* "api/v2/onboarding/signups"))

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
    (cons (gethash "client-id" yaml-file) (gethash "secret" yaml-file))))

(defun get-access-token (token-response)
  (getf (jonathan:parse token-response)
        :|access_token|))

(defun revoke-auth-token ()
  (setf *auth-token* nil))

(defun authenticate (&optional client-id secret)
  (setf *auth-token*
        (let* ((credentials (or (cons client-id secret) (credentials-from-yaml))))
          (get-access-token (dexador:post *authentication-uri*
                                          :basic-auth credentials
                                          :headers '(("Content-Type" . "application/x-www-form-urlencoded")))))))

(defun onboarding-signups-request-body (installation-id address-line &optional app-id)
  (jonathan:to-json (list :|installation_id| installation-id :|address_line| address-line :|app_id| app-id)))

(defun onboarding-signups (&key installation-id address-line app-id)
  (prettyprint-hash-table (jonathan:parse (dexador:post *onboarding-signups-uri*
                                                        :headers (list
                                                                  '("Content-Type" . "application/json")
                                                                  (cons "Authorization" (concatenate 'string "Bearer " token)))
                                                        :content (onboarding-signups-request-body installation-id address-line app-id))
                                          :as :hash-table)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt ()
  (format t "Pick one Incognia API to test:~%")
  (format t "1. POST /onboarding/signups~%")
  (format t "2. GET  /onboarding/signups*~%")
  (format t "3. POST /authentication/transactions*~%")
  (format t "*Not available yet~%")
  (let* ((api-index (parse-integer
                     (prompt-read "-->")
                     :junk-allowed t)))
    (case api-index
      (1 (progn
           (format t "Okay, let's call POST /onboarding/signups~%")
           (authenticate)
           (onboarding-signups :installation-id (prompt-read "--> installation-id")
                               :address-line (prompt-read "--> address-line")
                               :app-id (prompt-read "--> app-id"))))
      (t "Option not available."))))

;; Example
#+nil
(incognia-apis:onboarding-signups :installation-id "installation-id"
                                  :address-line "address-line")
