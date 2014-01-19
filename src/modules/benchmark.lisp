(cl-annot:enable-annot-syntax)
(in-package :statistics)

@export
(defmacro evaluation-time (form)
  "Returns time needed to evaluate a form."
  `(let ((start-time (get-internal-real-time)))
     ,form
     (/ (- (get-internal-real-time) start-time)
        internal-time-units-per-second)))

@export
(defun calculate-function-average-run-time (function number-of-runs &rest arguments)
  "Runs functon given number of times and then find an average time needed to run it once."
  (/ (evaluation-time
       (dotimes (i number-of-runs)
         (apply #'funcall function arguments)))
     number-of-runs))
