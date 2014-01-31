(cl-annot:enable-annot-syntax)
(in-package :statistics)

(defparameter *diagram-height* 250)
(defparameter *titles-list-up-gap* 20)
(defparameter *titles-list-step* 30)
(defparameter *titles-font-size* 17)

(defparameter *graph-step* 60)

(defstruct (Label (:type list))
  name
  value)

(defun label-width (font font-size label)
  (let ((bounding-box (vecto:string-bounding-box (label-name label) font-size font)))
    (- (svref bounding-box 2) (svref bounding-box 0))))

(defun find-longest-label (labels font font-size)
  (reduce #'max labels
          :key (curry #'label-width font font-size)))

@export
(defun draw-diagram (save-to data unit &optional (scale :logarithmic))
  (vecto:with-canvas (:width 1 :height 1)
    (let* ((font (vecto:get-font (make-pathname :directory '(:relative "res")
                                                :name "times"
                                                :type "ttf")))
           (titles-column-size (+ (ceiling (find-longest-label data font *titles-font-size*))
                                  65))) ; Magic number. Doesn't work without it.
      (vecto:with-canvas (:width (+ titles-column-size (* 65 (length data)))
                          :height *diagram-height*)
        (vecto:with-graphics-state
          (vecto:set-font font *titles-font-size*)
          (iter
            (for (title value) in data)
            (for y downfrom (- *diagram-height* *titles-list-up-gap*) by *titles-list-step*)
            (for i from 1)
            (after-each (vecto:draw-string 10 y (format nil "~D. ~A" i title)))))
        (iter
          (for (title value) in data)
          (for x upfrom titles-column-size by 65)
          (for i from 1)
          (for max-value next
               (let ((result (reduce #'max data :key #'label-value)))
                 (if (or (null result) (zerop result))
                   1
                   result)))
          (after-each
           (vecto:with-graphics-state
             (vecto:set-font font 15)
             (vecto:draw-centered-string x 10 (write-to-string i))
             (vecto:move-to x 30)
             (vecto:set-rgb-stroke 0.2 0.2 0.2)
             (vecto:set-line-width 40)
             (let* ((normalized (and (not (null value))
                                     (value-to-scale scale value max-value 170)))
                    (y (if (zerop normalized)
                         2
                         normalized)))
               (vecto:line-to x (+ 30 y))
               (vecto:set-font font *titles-font-size*)
               (vecto:draw-centered-string x
                                           (+ 40 y)
                                           (if (null value)
                                             "N/A"
                                             (format nil "~D ~A" value unit))))
             (vecto:stroke))))
        (vecto:save-png save-to)))))

@export
(defun draw-graph (save-to data units &optional (scale :logarithmic))
  (let* ((points-count (length data))
         (max-value (reduce #'max data :key #'label-value))
         (scale-size (- (* 30 points-count) 20)))
    (vecto:with-canvas (:width (* *graph-step* (1+ points-count))
                        :height (+ 30 (* 30 points-count)))
      (vecto:with-graphics-state
        (vecto:set-line-join :round)
        (vecto:set-line-cap :round)
        (vecto:set-line-width 4)
        (vecto:set-rgb-stroke 0.2 0.2 0.2)
        (let* ((label-value (label-value (first data)))
              (value (value-to-scale scale label-value max-value scale-size)))
          (vecto:move-to *graph-step* (+ 45 (ceiling value))))
        (iter
          (for (name value) in (rest data))
          (for y next (+ 45 (ceiling (value-to-scale scale value max-value scale-size))))
          (for x upfrom (* 2 *graph-step*) by *graph-step*)
          (after-each (vecto:line-to x y)))
        (vecto:stroke))
      (vecto:with-graphics-state
        (vecto:set-line-width 1)
        (vecto:set-rgb-stroke 0.2 0.2 0.2)
        (let ((font (vecto:get-font "res/times.ttf")))
          (iter
            (for (name value) in data)
            (for y next (+ 45 (ceiling (value-to-scale scale value max-value scale-size))))
            (for x upfrom 60 by *graph-step*)
            (after-each
             (vecto:set-font font 14)
             (vecto:draw-centered-string x 30 name)
             (vecto:set-font font 15)
             (vecto:draw-centered-string x 10
                                         (if (zerop value)
                                           "N/A"
                                           (format nil "~D ~A" value units)))
             (vecto:move-to x 45)
             (vecto:line-to x y)
             (vecto:stroke)))))
      (vecto:save-png save-to))))
