;;;; grid: sudoku problem instance representation.
(defpackage :sdku-rep
  (:documentation "Sudoku problem instance representation.")
  (:use :cl)
  (:export #:make-grid
           #:cell))

(in-package :sdku-rep)

(defclass grid ()
  ((mat :documentation "2D array for storing the grid values."
        :accessor mat :initarg :mat)))

;;; Instanciation
(defun make-grid (n)
  "Instanciate a grid of order `n`."
  (make-instance
   'grid :mat (make-array (list (* n n) (* n n)))))

;;; Access
(defmethod cell ((grid grid) i j)
  "Read the value in cell in row i column j."
  (with-slots ((mat mat)) grid
    (aref mat i j)))

(defun (setf cell) (new-val grid i j)
  "setf cell in row i, column j."
  (with-slots ((mat mat)) grid
    (setf (aref mat i j) new-val)))

;; Setfable places
;; (defmethod row )

;; (defmethod col )

;; (defmethod box )
