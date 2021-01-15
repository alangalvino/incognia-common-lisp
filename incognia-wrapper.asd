;;;; incognia-wrapper.asd

(asdf:defsystem #:incognia-wrapper
  :description "Incognia Common Lisp APIs Wrapper"
  :author "alan@incognia.com"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("dexador"
               "jonathan"
               "cl-yaml")
  :components ((:file "package")
               (:file "incognia-wrapper")))
