;;;; incognia-wrapper.lisp

(in-package #:incognia-wrapper)

(defvar *incognia-uri* "https://incognia.inloco.com.br/")

(defvar *authentication-uri* (concatenate 'string *incognia-uri* "api/v1/token"))

(defun get-access-token (token-response)
  (getf (jonathan:parse token-response)
        :|access_token|))

(defun authenticate (&optional (credentials (credentials-from-yaml)))
  (get-access-token (dexador:post *authentication-uri*
                                  :basic-auth credentials
                                  :headers '(("Content-type" . "application/x-www-form-urlencoded")))))
