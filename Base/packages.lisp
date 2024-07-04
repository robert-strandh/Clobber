(cl:in-package #:common-lisp-user)

(defpackage #:clobber
  (:use)
  (:export
   ;; conditions
   #:clobber-error
   #:transaction-log-not-open
      
   #:define-save-info
   #:open-transaction-log
   #:close-transaction-log
   #:transaction-log-open-p
   #:log-transaction
   #:with-transaction-log
   #:commit
   #:clear-uncommitted
   ;; utilities
   #:with-string-transaction-log
   #:serialize-to-string
   #:hash-table-to-alist
   #:make-a-hash-table))

(defpackage #:clobber-internal
  (:use #:common-lisp #:clobber)
  (:export
   #:load-transaction-log
   #:make-transaction-log
   #:serialize))
