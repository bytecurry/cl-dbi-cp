#|
  This file is a part of connection-pool project.
  Copyright (c) 2015 Thayne McCombs (bytecurry.software@gmail.com)
|#

#|
  Connection pool library for cl-dbi

  Author: Thayne McCombs (bytecurry.software@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-dbi-cp-asd
  (:use :cl :asdf))
(in-package :cl-dbi-cp-asd)

(defsystem cl-dbi-cp
  :version "0.1"
  :author "Thayne McCombs"
  :license "MIT"
  :depends-on (:cl-dbi
               :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "connection-pool"))))
  :description "Connection pool library for cl-dbi"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-dbi-cp-test))))
