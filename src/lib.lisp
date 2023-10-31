(in-package :cl-user)
(defpackage :dezero
  (:use :numcl))
(in-package :dezero)


(defclass dz-variable ()
  ((data :initarg :data)))


(defclass dz-function ()
  ((input)))

(defgeneric call (callable-object &rest arguments))
(defgeneric forward (self &rest arguments))
(defgeneric backward (self &rest arguments))

(defmethod call ((self dz-function) &rest arguments)
  (let* ((input (first arguments))
	 (x (slot-value input 'data))
	 (y (forward self x))
	 (output (make-instance 'dz-variable :data y)))
    (setf (slot-value self 'input) input)
    output))


(defclass dz-square (dz-function)
  ())

(defmethod forward ((self dz-square) &rest arguments)
  (let ((x (first arguments)))
    (* x x)))

(defmethod backward ((self dz-square) &rest arguments)
  (let* ((gy (first arguments))
	(x (slot-value (slot-value self 'input) 'data))
	(gx (* 2 x gy)))
    gx))


(defclass dz-exp (dz-function)
  ())

(defmethod forward ((self dz-exp) &rest arguments)
  (let ((x (first arguments)))
    (exp x)))

(defmethod backward ((self dz-exp) &rest arguments)
  (let* ((gy (first arguments))
	 (x (slot-value (slot-value self 'input) 'data))
	 (gx (* (exp x) gy)))
    gx))

(defclass composed-function (dz-function)
  ((first :initarg :first)
   (second :initarg :second)))

(defun compose-two (second first)
  (make-instance 'composed-function
		 :second second
		 :first first))

(defmethod call ((self composed-function) &rest arguments)
  (let* ((x (first arguments))
	 (g (slot-value self 'second))
	 (f (slot-value self 'first)))
    (call g (call f x))))

(defun numerical-diff (f x &optional (eps 1e-4))
  (let* ((x0 (make-instance 'dz-variable :data (- (slot-value x 'data) eps)))
	 (x1 (make-instance 'dz-variable :data (+ (slot-value x 'data) eps)))
	 (y0 (call f x0))
	 (y1 (call f x1))
	 )
    (/ (- (slot-value y1 'data) (slot-value y0 'data))
       (* 2 eps))))
