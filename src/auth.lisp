(in-package :cl-incognia)

(defvar *auth-token* nil)

(defun revoke-token ()
  (setf *auth-token* nil))

(defun token-expires-in ()
  (getf *auth-token* :|expires_in|))

(defun token-created-at ()
  (getf *auth-token* :|created_at|))

(defun token-type ()
  (getf *auth-token* :|token_type|))

(defun token-access-token ()
  (getf *auth-token* :|access_token|))

(defun authenticate ()
  (assert (and (client-id) (client-secret) (region)))
  (do-request
    :uri (incognia-uri *authentication-uri*)
    :method :post
    :basic-auth (credentials)
    :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
    :body (to-json '(:|grant_type| "client_credentials"))))

(defun auth-token-validp ()
  (let ((now (get-universal-time)))
    (and *auth-token* (token-access-token) (> (parse-integer (token-expires-in)) (- now (token-created-at))))))

(defun update-token ()
  (setf *auth-token* (authenticate))
  (setf (getf *auth-token* :|created_at|) (get-universal-time))
  *auth-token*)

(defun auth-token ()
  (if (auth-token-validp) *auth-token* (update-token)))
