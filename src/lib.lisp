(in-package :cl-user)
(defpackage :dezero
  (:use :cl)
  (:shadow
   :variable
   :function
   :exp))
(in-package :dezero)

(defgeneric call (callable-object &rest arguments))
(defgeneric forward (self &rest arguments))
(defgeneric backward (self &rest arguments))

(defclass variable ()
  ((data :initarg :data
	 :initform (error "supply data for variable"))
   (grad :initform nil)
   (creator :initarg :creator
	    :initform nil)))

(defmethod initialize-instance :after ((var variable) &key)
  (let ((data (slot-value var 'data)))
	(when data
	  (unless (typep data 'mgl-mat:mat)
	    (error (make-condition 'type-error
				   :datum data
				   :expected-type 'mgl-mat:mat))))))

(defmethod set-creator ((self variable) function)
  (setf (slot-value self 'creator) function))

(defmethod backward ((var variable) &rest arguments)
  (declare (ignore arguments))
  (unless (slot-value var 'grad)
    (setf (slot-value var 'grad)
	  (ones-like (slot-value var 'data))))
  (loop with funcs = (list (slot-value var 'creator))
	until (null funcs)
	do (let* ((f (pop funcs))
		  (x (slot-value f 'input))
		  (y (slot-value f 'output)))
	     (setf (slot-value x 'grad)
		   (backward f (slot-value y 'grad)))
	     (when (slot-value x 'creator)
	       (push (slot-value x 'creator)
		     funcs)))))



(defclass function ()
  ((input :initarg :input)
   (output :initarg :output)))

(defmethod call ((self function) &rest arguments)
  (let* ((input (first arguments))
	 (x (slot-value input 'data))
	 (y (forward self x))
	 (output (make-instance 'variable :data y)))
    (set-creator output self)
    (setf (slot-value self 'input) input)
    (setf (slot-value self 'output) output)
    output))


(defclass square (function)
  ())

(defmethod forward ((self square) &rest arguments)
  (let* ((x (first arguments))
	 (y (mgl-mat:make-mat (mgl-mat:mat-dimensions x))))
    (mgl-mat:gemm! 1 x x 0 y)
    y))

(defmethod backward ((self square) &rest arguments)
  (let* ((gy (first arguments))
	 (x (slot-value (slot-value self 'input) 'data))
	 (y (mgl-mat:make-mat (mgl-mat:mat-dimensions x))))
    (mgl-mat:gemm! 2 x gy 0 y)
    y))


(defclass exp (function)
  ())

(defmethod forward ((self exp) &rest arguments)
  (let* ((x (mgl-mat:copy-mat (first arguments)))
	 (y (mgl-mat:.exp! x)))
    y))

(defmethod backward ((self exp) &rest arguments)
  (let* ((gy (first arguments))
	 (x (mgl-mat:copy-mat
	     (slot-value (slot-value self 'input) 'data)))
	 (gx (mgl-mat:make-mat (mgl-mat:mat-dimensions x))))
    (mgl-mat:.exp! x)
    (mgl-mat:gemm! 1 x gy 0 gx)
    gx))

;; (defclass composed-function (function)
;;   ((first :initarg :first)
;;    (second :initarg :second)))

;; (defun compose-two (second first)
;;   (make-instance 'composed-function
;; 		 :second second
;; 		 :first first))

;; (defmethod call ((self composed-function) &rest arguments)
;;   (let* ((x (first arguments))
;; 	 (g (slot-value self 'second))
;; 	 (f (slot-value self 'first)))
;;     (call g (call f x))))

;; (defun numerical-diff (f x &optional (eps 1e-4))
;;   (let* ((x0 (make-instance 'variable :data (- (slot-value x 'data) eps)))
;; 	 (x1 (make-instance 'variable :data (+ (slot-value x 'data) eps)))
;; 	 (y0 (call f x0))
;; 	 (y1 (call f x1))
;; 	 )
;;     (/ (- (slot-value y1 'data) (slot-value y0 'data))
;;        (* 2 eps))))

(defun make-scalar (x)
  (mgl-mat:make-mat '(1 1) :initial-element x))

(defun make-variable (x)
  (make-instance 'variable
		 :data x))

(defun square (x)
  (call (make-instance 'square) x))

(defun exp (x)
  (call (make-instance 'exp) x))

(defun ones-like (x)
  (mgl-mat:make-mat (mgl-mat:mat-dimensions x)
		    :initial-element 1))
