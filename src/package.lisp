(defpackage :sdku-rep
  (:documentation "Sudoku problem instance representation.")
  (:use :cl)
  (:export #:make-grid
           #:cell
           #:pos
           #:order
           #:vals
           #:peers-row
           #:peers-col
           #:peers-box
           #:peers))
