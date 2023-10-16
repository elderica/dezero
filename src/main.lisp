(in-package :dezero)

(defun main ()
  (let* ((x (make-instance 'dz-variable :data (numcl:asarray 10)))
	 (f (make-instance 'square))
	 (y (funcall f x)))
    (slot-value y 'data)))
