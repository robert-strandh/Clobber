(cl:in-package #:asdf-user)

(defsystem :clobber
  :description "Library for transaction-oriented data bases."
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :license "FreeBSD, see file LICENSE.text"
  :depends-on (#:clobber-base)
  :serial t
  :components
  ((:file "Documentation/demo")
   (:file "Documentation/demo2"))
  :in-order-to ((test-op (test-op "clobber-test"))))
