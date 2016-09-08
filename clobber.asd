(cl:in-package #:asdf-user)

(defsystem :clobber
  :description "Library for transaction-oriented data bases."
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :license "FreeBSD, see file LICENSE.text"
  :serial t
  :components
  ((:file "packages")
   (:file "clobber")
   (:file "demo")
   (:file "demo2")))
