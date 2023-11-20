(in-package :dezero)

(let* ((x (make-variable
	   (make-scalar 0.5)))
       (y (square (exp (square x)))))
  (backward y)
  (slot-value x 'grad))
