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
  ((data :initarg :data)
   (grad :initform nil)
   (creator :initarg :creator
	    :initform nil)))

(defmethod set-creator ((self variable) function)
  (setf (slot-value self 'creator) function))

(defmethod backward ((var variable) &rest arguments)
  (declare (ignore arguments))
  (let ((f (slot-value var 'creator)))
    (when f
      (let ((x (slot-value f 'input)))
	(setf (slot-value x 'grad)
	      (backward f (slot-value var 'grad)))
	(backward x)))))


(defclass function ()
  ((input :initarg :input)
   (output :initarg :output)))

(defmethod call ((self function) &rest arguments)
  (let* ((input (first arguments))
	 (x (slot-value input 'data))
	 (y (forward self x))
	 (output (make-instance 'variable :data y)))
    ;(format t "call x: ~a~%" x)
    ;(format t "call y: ~a~%" y)
    (set-creator output self)
    (setf (slot-value self 'input) input)
    (setf (slot-value self 'output) output)
    output))


(defclass square (function)
  ())

(defmethod forward ((self square) &rest arguments)
  (let* ((x (first arguments))
	 (y (mgl-mat:make-mat (mgl-mat:mat-dimensions x))))
    ;(format t "forward before square x: ~a~%" x)
    (mgl-mat:gemm! 1 x x 0 y)
    ;(format t "forward after square y: ~a~%" y)
    y))

(defmethod backward ((self square) &rest arguments)
  (let* ((gy (first arguments))
	 (x (slot-value (slot-value self 'input) 'data))
	 (y (mgl-mat:make-mat (mgl-mat:mat-dimensions x))))
    ;(format t "backward before square x: ~a~%" x)
    ;(format t "backward before square gy: ~a~%" gy)
    (mgl-mat:gemm! 2 x gy 0 y)
    ;(format t "backward after square y: ~a~%" y)
    y))


(defclass exp (function)
  ())

(defmethod forward ((self exp) &rest arguments)
  ;(format t "forward before exp x: ~a~%" (first arguments))
  (let* ((x (mgl-mat:copy-mat (first arguments)))
	 (y (mgl-mat:.exp! x)))
    ;(format t "forward after exp y: ~a~%" y)
    y))

(defmethod backward ((self exp) &rest arguments)
  (let* ((gy (first arguments))
	 (x (mgl-mat:copy-mat
	     (slot-value (slot-value self 'input) 'data)))
	 (gx (mgl-mat:make-mat (mgl-mat:mat-dimensions x))))
    ;(format t "backward before exp x: ~a~%" x)
    (mgl-mat:.exp! x)
    (mgl-mat:gemm! 1 x gy 0 gx)
    ;(format t "backward after exp gx: ~a~%" gx)
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
