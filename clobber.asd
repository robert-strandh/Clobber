(cl:in-package #:asdf-user)

(defsystem :clobber
  :serial t
  :components
  ((:file "packages")
   (:file "clobber")
   (:file "demo")
   (:file "demo2")))
