(in-package :cl-incognia)

(defvar *auth-token* nil)

(defun revoke-auth-token ()
  (setf *auth-token* nil))

(defun token-expires-in (&optional (token *auth-token*))
  (getf token :|expires_in|))

(defun token-created-at (&optional (token *auth-token*))
  (getf token :|created_at|))

(defun token-type (&optional (token *auth-token*))
  (getf token :|token_type|))

(defun token-access-token (&optional (token *auth-token*))
  (getf token :|access_token|))

(defun set-token-created-at (&key (token *auth-token*) created-at)
  (setf (getf token :|created_at|) created-at)
  token)

(defun authenticate ()
  (assert (and (client-id) (client-secret) (region)))
  (do-request
    :uri (incognia-uri *authentication-uri*)
    :method :post
    :basic-auth (credentials)
    :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
    :body "grant_type=client_credentials"))

(defun auth-token-validp ()
  (let ((now (get-universal-time)))
    (and (token-access-token) (> (parse-integer (token-expires-in)) (- now (token-created-at))))))

(defun request-auth-token ()
  (let* ((auth-token (authenticate))
         (now (get-universal-time)))
    (set-token-created-at :token auth-token :created-at now)))

(defun fetch-auth-token ()
  (if (auth-token-validp)
      *auth-token*
      (setf *auth-token* (request-auth-token))))
