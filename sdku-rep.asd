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
                 (:file "grid" :depends-on ("package" "pos" "map"))))))
