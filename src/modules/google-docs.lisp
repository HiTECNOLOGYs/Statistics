(cl-annot:enable-annot-syntax)
(in-package :statistics)

(define-constant +tsv-regex+ "([^\\t]+)(?:\\t([^\\t][^\\n]+))?\\n"
  :test #'string=)

(define-constant +date-regex+ "(\\d{1,2})/(\\d{1,2})/(\\d{1,}) (\\d{1,2}):(\\d{1,2}):(\\d{1,2})"
  :test #'string=)

(defun parse-value (value dictionary)
  "Translates value to symbol based on given dictionary."
  (cdr (assoc value dictionary
              :test #'string=)))

(defun parse-date (value)
  "Parses timestamp from string."
  (let ((matches (nth-value 1 (cl-ppcre:scan-to-strings +date-regex+ value))))
    (if (null matches)
      value
      (list :day (parse-integer (aref matches 1))
            :month (parse-integer (aref matches 0))
            :year (parse-integer (aref matches 2))
            :hour (parse-integer (aref matches 3))
            :minute (parse-integer (aref matches 4))
            :second (parse-integer (aref matches 5))))))

(defun make-record (headers &rest columns)
  "Makes record out of columns."
  (iter
    (for header in headers)
    (for column in columns)
    (nconcing (list header column))))

@export
(defun parse-tsv (data columns-ids)
  "Parses tab separated values."
  (let (result columns-headers)
    (cl-ppcre:do-register-groups (timestamp columns) (+tsv-regex+ data)
      (if (null columns-headers)
        (setf columns-headers (cons timestamp columns))
        (push (apply #'make-record
                     (cons :date columns-ids)
                     (parse-date timestamp)
                     (cl-ppcre:split "\\s*\\t\\s*" columns))
              result)))
    (values (reverse result)
            columns-headers)))
