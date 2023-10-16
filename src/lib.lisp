(in-package :cl-user)
(defpackage :dezero
  (:use :cl))
(in-package :dezero)

(defclass dz-variable ()
  ((data :initarg :data)))

(defgeneric call (callable-object &rest arguments))

(defclass has-call (standard-object function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((self has-call) &key)
  (closer-mop:set-funcallable-instance-function
   self
   #'(lambda (&rest arguments)
       (apply #'call self arguments))))

(defmethod call ((self has-call) &rest arguments)
  (declare (ignore arguments))
  (cerror "Return nil"
	  "class ~a does not define a call method"
	  (class-name (class-of self)))
  nil)

(defclass dz-function (has-call)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defgeneric forward (self &rest arguments))

(defmethod call ((self dz-function) &rest arguments)
  (let* ((x (slot-value (first arguments) 'data))
	 (y (forward self x)))
    (make-instance 'dz-variable :data y)))

(defmethod forward ((self dz-function) &rest arguments)
  (declare (ignore arguments))
  (cerror "Return nil"
	  "class ~a does not define a forward method"
	  (class-name (class-of self))))

(defclass square (dz-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod forward ((self dz-function) &rest arguments)
  (let ((x (first arguments)))
    (numcl:expt x (numcl:asarray '(2)))))
