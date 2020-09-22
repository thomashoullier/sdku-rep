;;;; grid: sudoku problem instance representation.
(in-package :sdku-rep)

(defclass grid ()
  ((mat :documentation "2D array for storing the grid values."
        :accessor mat :initarg :mat)
   (pmap :documentation "Reference to problem map: peers of cells."
         :accessor pmap :initarg :pmap)))

;;; Instanciation
(defun make-grid (n)
  "Instantiate a grid of order n."
  (make-instance 'grid
                 :mat (make-array (list (* n n) (* n n)))
                 :pmap (make-glob-map n)))

;;; Access
(defmethod cell ((grid grid) pos)
  "Read the value in cell at pos."
  (with-slots ((mat mat)) grid
    (posref mat pos)))

(defun (setf cell) (new-val grid pos)
  "setf cell at pos."
  (with-slots ((mat mat)) grid
    (setf (posref mat pos) new-val)))

;;; Problem structure information.
;; TODO: * Factor out (slot-value grid 'pmap).
(defmethod order ((grid grid))
  "Return the order of the grid."
  (slot-value (slot-value grid 'pmap) 'order))

(defmethod vals ((grid grid))
  "Return allowable values for the grid."
  (slot-value (slot-value grid 'pmap) 'vals))

(defmethod peers-row ((grid grid) pos)
  "Peers in row of a given pos in grid."
  (with-slots ((mat peers-row-mat)) (slot-value grid 'pmap)
    (posref mat pos)))

(defmethod peers-col ((grid grid) pos)
  "Peers in col of a given pos in grid."
  (with-slots ((mat peers-col-mat)) (slot-value grid 'pmap)
    (posref mat pos)))

(defmethod peers-box ((grid grid) pos)
  "Peers in box of a given pos in grid."
  (with-slots ((mat peers-box-mat)) (slot-value grid 'pmap)
    (posref mat pos)))

(defmethod peers ((grid grid) pos)
  "All peers of a given pos in grid."
  (with-slots ((mat peers-all-mat)) (slot-value grid 'pmap)
    (posref mat pos)))

;; Setfable places
;; (defmethod row )

;; (defmethod col )

;; (defmethod box )
