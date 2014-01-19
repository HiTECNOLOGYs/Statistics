(cl-annot:enable-annot-syntax)
(in-package :statistics)

(define-constant +tsv-regex+ "([^\\t]+)\\t([^\\t]+)\\t([^\\t]+)\\t([^\\t]+)\\t([^\\t]+)\\t([^\\t]+)\\n"
  :test #'equal)

@export
(defun parse-tsv (data)
  "Parses tab separated values."
  (let (result)
    (cl-ppcre:do-register-groups
        (timestamp emacs-version emacs-installed-from slime-installed-from os os-version)
        (+tsv-regex+ data)
      (push (list :date timestamp
                  :emacs-version emacs-version
                  :emacs-installed-from emacs-installed-from
                  :slime-installed-from slime-installed-from
                  :os os
                  :os-version os-version)
            result))
    (reverse result)))
