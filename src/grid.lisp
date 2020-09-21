;;;; grid: sudoku problem instance representation.
(in-package :sdku-rep)

(defclass grid ()
  ((mat :documentation "2D array for storing the grid values."
        :accessor mat :initarg :mat)
   (order :documentation "Order of the grid (typical sudoku is 3)."
          :accessor order :initarg :order)
   (pmap :documentation "Reference to problem map: peers of cells."
         :accessor pmap :initarg :pmap)))

;;; Instanciation
(defun make-grid (n)
  ;; TODO: * Instantiate a map of peers for the order if it does not exist.
  "Instantiate a grid of order n."
  (make-instance 'grid
                 :mat (make-array (list (* n n) (* n n)))
                 :order n))

;;; Access
(defmethod cell ((grid grid) pos)
  "Read the value in cell at pos."
  (with-slots ((mat mat)) grid
    (posref mat pos)))

(defun (setf cell) (new-val grid pos)
  "setf cell at pos."
  (with-slots ((mat mat)) grid
    (setf (posref mat pos) new-val)))

;; TODO: * Remove these once reader macro is implemented.
(defmethod celli ((grid grid) row col)
  "Read cell by row and col rather than pos."
  (with-slots ((mat mat)) grid
    (aref mat row col)))

(defun (setf celli) (new-val grid row col)
  "setf cell at row, col."
  (with-slots ((mat mat)) grid
    (setf (aref mat row col) new-val)))

;;; Peers of cells
(defmethod peers-row ((grid grid) pos)
  "Peers in row of a given pos in grid."

  )

(defmethod peers-col ((grid grid) pos)
  "Peers in col of a given pos in grid."

  )

(defmethod peers-box ((grid grid) pos)
  "Peers in box of a given pos in grid."

  )

(defmethod peers ((grid grid) pos)
  "All peers of a given pos in grid."

  )

;; Setfable places
;; (defmethod row )

;; (defmethod col )

;; (defmethod box )
