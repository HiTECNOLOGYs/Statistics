(defsystem :statistics
  :description "Small kit of tools that I use for statistical analysis and researches."
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :licence "Public Domain"
  :depends-on (:alexandria
               :iterate
               :vecto
               :hctsmsl
               :cl-ppcre
               :cl-annot)
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "math")
               (:file "graphics")
               (:module "modules"
                        :components ((:file "benchmark")
                                     (:file "google-docs")))
               (:file "ui")))
