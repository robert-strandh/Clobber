(cl:in-package #:asdf-user)

(defsystem #:clobber-test
  :depends-on (#:clobber)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "test-serialize")
   (:file "test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:clobber-test '#:run-tests)))
