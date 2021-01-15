;;;; incognia-wrapper.lisp

(in-package #:incognia-wrapper)

(defvar *incognia-uri* "https://incognia.inloco.com.br/")

(defvar *authentication-uri* (concatenate 'string *incognia-uri* "api/v1/token"))

(defvar *onboarding-signups-uri* (concatenate 'string *incognia-uri* "api/v2/onboarding/signups"))

(defun parse-yaml-file ()
  (yaml:parse (asdf:system-relative-pathname :incognia-wrapper #p"./credentials.yaml")))

(defun credentials-from-yaml ()
  (let* ((yaml-file (parse-yaml-file)))
    (cons (gethash "client-id" yaml-file) (gethash "secret" yaml-file))))

(defun get-access-token (token-response)
  (getf (jonathan:parse token-response)
        :|access_token|))

(defun authenticate (&optional (credentials (credentials-from-yaml)))
  (get-access-token (dexador:post *authentication-uri*
                                  :basic-auth credentials
                                  :headers '(("Content-type" . "application/x-www-form-urlencoded")))))


(defun onboarding-signups-request-body (installation-id address-line &optional app-id)
  (jonathan:to-json (list :|installation_id| installation-id :|address_line| address-line :|app_id| app-id)))

;; TODO: add GET method
;; TODO: use keyword arguments
(defun onboarding-signups (installation-id address-line)
  (let* ((token (authenticate)))
    (dexador:post *onboarding-signups-uri*
                  :verbose t
                  :headers (list
                            '("Content-Type" . "application/json")
                            (cons "Authorization" (concatenate 'string "Bearer " token)))
                  :content (onboarding-signups-request-body installation-id address-line))))

;; Example
(incognia-apis:onboarding-signups "installation-id"
                                   "address-line")
