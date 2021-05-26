(defsystem "cl-incognia"
  :version "0.1.0"
  :author "alan alvino"
  :license "MIT"
  :depends-on (:dexador
               :jonathan)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "error")
                 (:file "http")
                 (:file "routes")
                 (:file "address")
                 (:file "config")
                 (:file "auth")
                 (:file "main"))))
  :description "Incognia API Common Lisp Client"
  :in-order-to ((test-op (test-op "cl-incognia/tests"))))

(defsystem "cl-incognia/tests"
  :author "alan alvino"
  :license "MIT"
  :depends-on (:cl-incognia
               :rove)
  :components ((:module "tests"
                :serial t
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "address")
                 (:file "config")
                 (:file "auth")
                 (:file "main"))))
  :description "Test system for cl-incognia"
  :perform (test-op (op c) (symbol-call :rove :run c)))
