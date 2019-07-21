#|
  This file is a part of robot project.
  Copyright (c) 2019 Connor Langlois (connor.langlois@maine.edu)
|#

#|
  Author: Connor Langlois (connor.langlois@maine.edu)
|#

(defsystem "robot"
  :version "0.1.0"
  :author "Connor Langlois"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "robot"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "../README.md"))
  :in-order-to ((test-op (test-op "robot-test"))))
