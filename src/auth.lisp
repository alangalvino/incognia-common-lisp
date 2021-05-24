(in-package :incognia-wrapper)

(defvar *auth-token* nil)

(defun revoke-token ()
  (setf *auth-token* nil))

(defun token-expires-in ()
  (getf *auth-token* :|expires_in|))

(defun token-created-at ()
  (getf *auth-token* :|created_at|))

(defun access-token ()
  (getf *auth-token* :|access_token|))

(defun auth-token-validp ()
  (let ((now (get-universal-time)))
    (and *auth-token* (access-token) (> (parse-integer token-expires-in) (- now (token-created-at))))))

(defun update-token ()
  (setf *auth-token* (authenticate))
  (setf (getf *auth-token* :|created_at|) (get-universal-time))
  *auth-token*)

(defun auth-token ()
  (if (auth-token-validp) *auth-token* (update-token)))
