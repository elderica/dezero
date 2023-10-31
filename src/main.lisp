(in-package :dezero)

(defun main ()
  (let* ((x (make-instance 'dz-variable :data (numcl:asarray 10)))
	 (f (make-instance 'square))
	 (y (call f x)))
    (slot-value y 'data)))
