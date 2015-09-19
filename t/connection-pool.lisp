(in-package :cl-user)
(defpackage connection-pool-test
  (:use :cl
        :connection-pool
        :prove))
(in-package :connection-pool-test)

;; NOTE: To run this test file, execute `(asdf:test-system :connection-pool)' in your Lisp.

(plan nil)



(finalize)
