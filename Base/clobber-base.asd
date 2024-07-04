(defsystem #:clobber-base
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "protocol")
   (:file "utilities")
   (:file "methods")
   (:file "serialize-methods")
   (:file "docstrings")))

