(defpackage :statistics.benchmark
  (:use :cl
        :statistics)
  (:export :evaluation-time
           :function-average-run-time))

(in-package :statistics.benchmark)

(defmacro evaluation-time (form)
  "Returns time needed to evaluate a form."
  `(let ((start-time (get-internal-real-time)))
     ,form
     (/ (- (get-internal-real-time) start-time)
        internal-time-units-per-second)))

(defun function-average-run-time (function number-of-runs &rest arguments)
  "Runs functon given number of times and then finds average time needed to run
it once."
  (/ (evaluation-time
       (dotimes (i number-of-runs)
         (apply #'funcall function arguments)))
     number-of-runs))
