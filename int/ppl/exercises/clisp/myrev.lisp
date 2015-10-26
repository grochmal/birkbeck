; common lisp reverse
(defun switch (f) (lambda (x y) (funcall f y x)))
(setf *recons* (switch 'cons))
(defun myrev (l) (reduce *recons* (cons () l)))

