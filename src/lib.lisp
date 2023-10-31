(in-package :cl-user)
(defpackage :dezero
  (:use :cl))
(in-package :dezero)


(defclass dz-variable ()
  ((data :initarg :data)))


(defclass dz-function ()
  ())
(defgeneric call (callable-object &rest arguments))
(defgeneric forward (self &rest arguments))

(defmethod call ((self dz-function) &rest arguments)
  (let* ((x (slot-value (first arguments) 'data))
	 (y (forward self x)))
    (make-instance 'dz-variable :data y)))

(defclass square (dz-function)
  ())

(defmethod forward ((self dz-function) &rest arguments)
  (let ((x (first arguments)))
    (numcl:expt x (numcl:asarray '(2)))))
