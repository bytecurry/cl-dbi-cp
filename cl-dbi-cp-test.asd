#|
  This file is a part of connection-pool project.
  Copyright (c) 2015 Thayne McCombs (bytecurry.software@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-dbi-cp-test-asd
  (:use :cl :asdf))
(in-package :cl-dbi-cp-test-asd)

(defsystem cl-dbi-cp-test
  :author "Thayne McCombs"
  :license "MIT"
  :depends-on (:cl-dbi-cp
               :bordeaux-threads
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "connection-pool"))))
  :description "Test system for connection-pool"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op (op c)
                    (uiop:symbol-call :prove-asdf '#:run c)))
