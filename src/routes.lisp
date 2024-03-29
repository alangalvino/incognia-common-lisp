(in-package :cl-incognia)

;; Incognia APIs URIs
(defvar *incognia-br-uri* "https://incognia.inloco.com.br")
(defvar *incognia-us-uri* "https://api.us.incognia.com")

;; Incognia APIs Resource URIs
(defvar *authentication-uri* "api/v1/token")
(defvar *signups-uri* "api/v2/onboarding/signups")
(defvar *transactions-uri* "api/v2/authentication/transactions")
(defvar *feedbacks-uri* "api/v2/feedbacks")

(defun incognia-uri (resource-uri)
  (concatenate 'string (if (us-regionp)
                           *incognia-us-uri*
                           *incognia-br-uri*) "/" resource-uri))
