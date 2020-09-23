(defsystem sdku-rep
  :name "sdku-rep"
  :version "0.1"
  :author "Thomas Houllier"
  :license "MIT"
  :description "Sudoku problems internal representation."
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "pos" :depends-on ("package"))
                 (:file "map" :depends-on ("package" "pos"))
                 (:file "grid" :depends-on ("package" "pos" "map")))))
  :in-order-to ((test-op (test-op "sdku-rep/test"))))

(defsystem sdku-rep/test
  :name "sdku-rep/test"
  :description "Rove test suite for sdku-rep."
  :depends-on ("sdku-rep" "rove")
  :components
  ((:module "test"
    :components ((:file "suite"))))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
