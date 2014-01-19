(cl-annot:enable-annot-syntax)
(in-package :statistics)

@export
(defun normalize (value max-value &optional (scale-size 1))
  "Transforms values to a single scale."
  (* (/ value max-value) scale-size))

@export
(defun to-log-scale (value max-value &optional (scale-size 1) base)
  "Converts value to logarithmic scale."
  (* (/ (if (not (numberp base))
          (log value)
          (log value base))
        (if (not (numberp base))
          (log max-value)
          (log max-value base)))
     scale-size))
