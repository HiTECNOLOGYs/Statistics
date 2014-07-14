(defsystem :statistics
  :description "Small kit of tools that I use for statistical analysis and researches."
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :licence "Public Domain"
  :depends-on (:alexandria
               :iterate
               :vecto)
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "math")
               (:file "graphics")
               (:file "ui")))

(defsystem :statistics/benchmark
  :description "Benchmarking module for Statistics."
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :depends-on (:statistics)
  :pathname "src/modules/"
  :components ((:file "benchmark")))

(defsystem :statistics/google-docs
  :description "Module for parsing Google Docs tables exported in TSV format."
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :depends-on (:statistics
               :cl-ppcre)
  :pathname "src/modules/"
  :components ((:file "google-docs")))
