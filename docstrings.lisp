(cl:in-package #:clobber)

(setf
 (documentation #'open-transaction-log 'function)
 "Load transaction log from FILENAME and return an instance of `clobber:transaction-log'.

FUNCTION should accept a single argument, a transaction object (see
`clobber:load-transaction-log').")

(setf
 (documentation #'load-transaction-log 'function)
 "Read transaction log from FILENAME, calling FUNCTION for each transaction.

FUNCTION should accept a single argument, a transaction object.

OBJECT-TABLE should be a hash table.")
