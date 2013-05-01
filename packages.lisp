(cl:in-package #:common-lisp-user)

(defpackage #:clobber
  (:use #:common-lisp)
  (:export
   #:define-save-info
   #:open-transaction-log
   #:close-transaction-log
   #:log-transaction))
