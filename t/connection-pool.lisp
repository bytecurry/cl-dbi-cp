(in-package :cl-user)
(defpackage connection-pool-test
  (:use :cl
        :cl-dbi-cp
        :dbi
        :dbi.driver
        :prove)
  (:import-from :bt
                #:make-thread))
(in-package :connection-pool-test)

;; NOTE: To run this test file, execute `(asdf:test-system :connection-pool)' in your Lisp.

(defvar *pool*)
(defvar *open-connections* 0)
(defvar *disconnect-count* 0)
(defvar *conn*)

(defun open-connections ()
  *open-connections*)
(defun num-connections-created ()
  (+ *open-connections* *disconnect-count*))
(defun reset-count ()
  (setf *open-connections* 0)
  (setf *disconnect-count* 0))

(defmacro pool-test (name (&rest pool-call) &body body)
  `(subtest ,name
     (let* ((*open-connections* 0)
            (*disconnect-count* 0)
            (*pool* ,pool-call))
       ,@body)))

(defclass <dbd-fakedb> (<dbi-driver>) ())
(defclass <fake-connection> (<dbi-connection>) ())

(defmethod disconnect ((conn <fake-connection>))
  (incf *disconnect-count*)
  (decf *open-connections*))

(defmethod make-connection ((driver <dbd-fakedb>) &rest params &key database-name)
  (declare (ignore params))
  (incf *open-connections*)
  (make-instance '<fake-connection> :database-name database-name))

(plan 3)

(pool-test "simple-connection-pool" (make-simple-connection-pool :fakedb :database-name "my-db")
  (setf *conn* (request-connection *pool*))
  (release-connection *pool* *conn*)
  (is (open-connections) 1)
  (handler-case (progn
                  (setf *conn* (request-connection *pool*))
                  (release-connection *pool* *conn*))
    (new-connection ()
      (fail "Created new connection")))
  (is (open-connections) 1)
  (let ((conn1 (request-connection *pool*))
        (conn2 (request-connection *pool*)))
    (is (open-connections) 2)
    (release-connection *pool* conn2)
    (release-connection *pool* conn1)
    (is (open-connections) 2))
  (with-pooled-connection (c1 *pool*)
    (with-pooled-connection (c2 *pool*)
      (with-pooled-connection (c3 *pool*)
        (is (open-connections) 3))))
  (with-pooled-connection (c *pool*)
    (is (open-connections) 3))
  (is (num-connections-created) 3))

(pool-test "elastic-connection-pool" (make-elastic-connection-pool 2 :fakedb :database-name "my-db")
  (setf *conn* (request-connection *pool*))
  (release-connection *pool* *conn*)
  (is (open-connections) 1)
  (with-pooled-connection (c1 *pool*)
    (with-pooled-connection (c2 *pool*)
      (with-pooled-connection (c3 *pool*)
        (is (open-connections) 3))
      (with-pooled-connection (c3 *pool*)
        (is (open-connections) 3)
        (with-pooled-connection (c4 *pool*)
          (is (open-connections) 4)
          (is *disconnect-count* 0))))
    (is *disconnect-count* 1))
  (is (open-connections) 2)
  (is *disconnect-count* 2)
  (is (num-connections-created) 4))

(pool-test "fixed-size-connection-pool" (make-fixed-size-connection-pool 2 :fakedb :database-name "my-db")
  (is (open-connections) 2)
  (handler-case
      (progn
        (setf *conn* (request-connection *pool*))
        (with-pooled-connection (c2 *pool*)
          (is (open-connections) 2))
        (release-connection *pool* *conn*))
    (new-connection ()
      (fail "Connection created on request.")))
  (let ((conn1 (request-connection *pool*))
        (conn2 (request-connection *pool*)))
    (make-thread
     (let ((pool *pool*))
       (lambda ()
         (sleep 0.25)
         (release-connection pool conn1))))
    (with-pooled-connection (c3 *pool*)
      ;; We shouldn't get to this point until after the thread has released
      ;; conn1, so we should have acquired conn1 again.
      (is c3 conn1 :test #'eq "Should acquire the connection that was released in another thread."))
    (release-connection *pool* conn2))
  (is (open-connections) 2)
  (is *disconnect-count* 0)
  (is (num-connections-created) 2))

(finalize)
