(cl:in-package #:common-lisp-user)

(asdf:defsystem :clobber
  :components
  ((:file "packages")
   (:file "clobber" :depends-on ("packages"))
   (:file "demo" :depends-on ("clobber"))
   (:file "demo2" :depends-on ("clobber"))))
