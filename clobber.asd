(cl:in-package #:asdf-user)

(defsystem :clobber
  :components
  ((:file "packages")
   (:file "clobber" :depends-on ("packages"))
   (:file "demo" :depends-on ("clobber"))
   (:file "demo2" :depends-on ("clobber"))))
