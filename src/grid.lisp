;;;; grid: sudoku problem instance representation.
(in-package :sdku-rep)

(defclass grid ()
  ((mat :documentation "2D array for storing the grid values."
        :accessor mat :initarg :mat)
   (order :documentation "Order of the grid (typical sudoku is 3)."
          :accessor order :initarg :order)))

;;; Instanciation
(defun make-grid (n)
  ;; TODO: * Instanciate a map of peers for the order if it does not exist.
  "Instanciate a grid of order n."
  (make-instance 'grid
                 :mat (make-array (list (* n n) (* n n)))
                 :order n))

;;; Access
(defmethod cell ((grid grid) pos)
  "Read the value in cell at pos."
  (with-slots ((mat mat)) grid
    (aref mat (pos-row pos) (pos-col pos))))

(defun (setf cell) (new-val grid pos)
  "setf cell at pos."
  (with-slots ((mat mat)) grid
    (setf (aref mat (pos-row pos) (pos-col pos)) new-val)))

(defmethod celli ((grid grid) row col)
  "Read cell by row and col rather than pos."
  (with-slots ((mat mat)) grid
    (aref mat row col)))

(defun (setf celli) (new-val grid row col)
  "setf cell at row, col."
  (with-slots ((mat mat)) grid
    (setf (aref mat row col) new-val)))

;; Setfable places
;; (defmethod row )

;; (defmethod col )

;; (defmethod box )
