(in-package :cl-user)
(defpackage cl-dbi-cp
  (:nicknames :cp)
  (:use :cl :cl-dbi)
  (:export #:connection-pool
           ;; interface
           #:request-connection
           #:release-connection
           #:connection-pool-params
           #:with-pooled-connection
           ;; concrete classes
           #:simple-connection-pool
           #:make-simple-connection-pool
           #:elastic-connection-pool
           #:make-elastic-connection-pool
           #:fixed-size-connection-pool
           #:make-fixed-size-connection-pool
           ;; special accessors
           #:elastic-connection-pool-max
           #:connection-pool-size
           ;; functions for third-party pools
           #:with-locked-connection-pool
           #:make-pooled-connection))
(in-package :cl-dbi-cp)

(defclass connection-pool ()
  ((params :accessor connection-pool-params
           :initarg :params
           :type list)
   (lock :accessor connection-pool-lock
         :initform (bt:make-recursive-lock)))
  (:documentation "Abstract class for an object that manages a connection pool."))

(defgeneric request-connection (pool)
  (:documentation "Request a connection from a connection pool."))

(defgeneric release-connection (pool connection)
  (:documentation "Release a connection to a connection pool so that it
is available to other clients."))

(defun make-pooled-connection (pool)
  "Create a new connection for a pool. (Always creates a new new one).
Should not be called externally."
  (declare (connection-pool pool))
  (apply #'connect (connection-pool-params pool)))

(defmacro with-pooled-connection ((var pool) &body body)
  "Execute body with VAR bound to a connection from the pool POOL."
  `(let ((,var (request-connection ,pool)))
     (unwind-protect
          (progn ,@body)
       (release-connection ,pool ,var))))

(defmacro with-locked-connection-pool ((pool) &body body)
  "Execute body with a lock on the connection pool"
  `(bt:with-recursive-lock-held ((connection-pool-lock ,pool))
     ,@body))

;;; simple pool

(defclass simple-connection-pool (connection-pool)
  ((available :accessor connection-pool-available
              :initform nil
              :type list))
  (:documentation "A simple connection pool that will always
create a new connection if there isn't one available, but will cache
old, unused connections forever."))

(defun make-simple-connection-pool (driver-name &rest params)
  "Create an instance of simple-connection-pool. The connections will
be created with the supplied parameters."
  (make-instance 'simple-connection-pool
                 :params (list* driver-name params)))

(defmethod request-connection ((pool simple-connection-pool))
  (or (with-locked-connection-pool (pool)
        (pop (connection-pool-available pool)))
      (make-pooled-connection pool)))

(defmethod release-connection ((pool simple-connection-pool) connection)
  (with-locked-connection-pool (pool)
    (push connection (connection-pool-available pool))))

;; elastic pool

(defclass elastic-connection-pool (simple-connection-pool)
  ((max :accessor elastic-connection-pool-max
        :initarg :max
        :initform (error "Max is required")
        :type (integer 0))
   (%count :initform 0
           :type (integer 0)))
  (:documentation "An elastic connection pool that never has more than MAX free
connections."))

(defun make-elastic-connection-pool (max-free driver-name &rest params)
  "Create an instance of ELASTIC-CONNECTION-POOL with a maximum of MAX-FREE
free connections."
  (make-instance 'elastic-connection-pool
                 :max max-free
                 :params (list* driver-name params)))

(defmethod request-connection ((pool elastic-connection-pool))
  (with-slots (%count available) pool
    (or (with-locked-connection-pool (pool)
          (unless (zerop %count)
            (decf %count)
            (pop available)))
        (make-pooled-connection pool))))

(defmethod release-connection ((pool elastic-connection-pool) connection)
  (with-slots (%count max available) pool
    (let (should-disconnect)
      (with-locked-connection-pool (pool)
        (if (= %count max)
            (setf should-disconnect t)
            (progn
              (incf %count)
              (push connection available))))
      (when should-disconnect
        (disconnect connection)))))


;; fixed size pool

(defclass fixed-size-connection-pool (simple-connection-pool)
  ((size :accessor connection-pool-size
         :initarg :size
         :initform (error "Size is required")
         :type (integer 0))
   (%condition :initform (bt:make-condition-variable))
   (%count :initform 0
           :type (integer 0)))
  (:documentation "A connection pool with a fixed number of connections.
If all connections are in use request-connection will block until one becomes available."))

(defun make-fixed-size-connection-pool (size driver-name &rest params)
  "Create an instance of FIXED-SIZE-CONNECTION-POOL with a size of SIZE connections."
  (make-instance 'fixed-size-connection-pool
                 :size size
                 :params (list* driver-name params)))

(defmethod initialize-instance :after ((pool fixed-size-connection-pool) &rest args &key)
  "Initially create all of the connections for the pool"
  (declare (ignore args))
  (with-slots (%count size available) pool
    (dotimes (i size)
      (push (make-pooled-connection pool) available))
    (setf %count size)))

(defmethod (setf connection-pool-size) (new-size (pool fixed-size-connection-pool))
  "When the user changes the size, we create or remove enough connection to change the
size. This has to do quite a bit of work while holding a lock, so don't do it very often."
  (with-slots (%count size available) pool
    (with-locked-connection-pool (pool)
      (let ((delta (- new-size size)))
        (cond
          ((plusp delta)
           (dotimes (i delta)
             (push (make-pooled-connection pool) available))
           (incf %count delta)
           (bt:condition-notify (slot-value pool '%condition)))
          ((minusp delta)
           (loop repeat (- delta)
              while available
              for conn = (pop available)
              do (disconnect conn)
              do (decf %count))))
        (setf size new-size)))))

(defmethod request-connection ((pool fixed-size-connection-pool))
  (with-slots (%count %condition available) pool
    (with-locked-connection-pool (pool)
      (loop while (zerop %count)
         do (bt:condition-wait %condition
                               (connection-pool-lock pool)))
      (decf %count)
      (pop available))))

(defmethod release-connection ((pool fixed-size-connection-pool) connection)
  (with-slots (%count %condition available size) pool
    (let (should-disconnect)
      (with-locked-connection-pool (pool)
        (if (= %count size)
            (setf should-disconnect t)
            (progn
              (incf %count)
              (push connection available)
              (bt:condition-notify %condition))))
      (when should-disconnect
        (disconnect connection)))))
