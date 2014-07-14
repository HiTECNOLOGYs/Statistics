(in-package :statistics)

(defun to-linear-scale (value max-value scale-size)
  "Transforms values to a single linear scale."
  (* (/ value max-value) scale-size))

(defun to-log-scale (value max-value scale-size &optional base)
  "Converts value to logarithmic scale."
  (* (/ (if (not (numberp base))
          (log value)
          (log value base))
        (if (not (numberp base))
          (log max-value)
          (log max-value base)))
     scale-size))

(defun value-to-scale (scale value max-value scale-size &rest other-params)
  "Converts value to specified scale.
Scales available:
* Logarithmic (:logarithmic)
* Linear (:linear)"
  (case scale
    (:logarithmic (apply #'to-log-scale value max-value scale-size other-params))
    (:linear      (apply #'to-linear-scale value max-value scale-size other-params))
    (otherwise    (error "Unknown scale: ~S" scale))))
