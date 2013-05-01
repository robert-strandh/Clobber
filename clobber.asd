(cl:in-package #:common-lisp-user)

(asdf:defsystem :clobber
  :components
  ((:file "packages")
   (:file "clobber" :depends-on ("packages"))))
