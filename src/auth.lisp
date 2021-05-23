(in-package :incognia-wrapper)

(defvar *auth-token* nil)

(defun revoke-token ()
  (setf *auth-token* nil))

(defun auth-token-valid-p ()
  (let ((expires-in (parse-integer (getf *auth-token* :|expires_in|)))
        (created-at (getf *auth-token* :|created_at|))
        (now (get-universal-time)))
    (and *auth-token* (> expires-in (- now created-at)))))

(defun update-token ()
  (setf *auth-token* (authenticate))
  (setf (getf *auth-token* :|created_at|) (get-universal-time))
  *auth-token*)

(defun auth-token ()
  (if (and *auth-token* (getf *auth-token* :|access_token|) (auth-token-valid-p))
      *auth-token*
      (update-token)))
