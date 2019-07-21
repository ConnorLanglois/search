#|
  This file is a part of queens project.
  Copyright (c) 2019 Connor Langlois (connor.langlois@maine.edu)
|#

#|
  Author: Connor Langlois (connor.langlois@maine.edu)
|#

(defsystem "queens"
  :version "0.1.0"
  :author "Connor Langlois"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "queens"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "../README.md"))
  :in-order-to ((test-op (test-op "queens-test"))))
