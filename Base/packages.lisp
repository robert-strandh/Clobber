(cl:in-package #:common-lisp-user)

(defpackage #:clobber
  (:use)
  (:export
   #:define-save-info
   #:open-transaction-log
   #:close-transaction-log
   #:transaction-log-open-p
   #:log-transaction
   #:with-transaction-log))

(defpackage #:clobber-internal
  (:use #:common-lisp #:clobber)
  (:export
   #:load-transaction-log
   #:make-transaction-log
   #:serialize))
