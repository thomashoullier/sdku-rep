;;;; Map holds the general logical structure of problems: which pos are peers
;;;; of a given pos in a given unit.
(in-package :sdku-rep)

;; Note:
;; * Peers are the other pos of a given pos in its unit
;; (row, col, box or all of them).
;; * A pos is not its own peer.
;; * A map is true for all grid instances of a given order.
(defclass map ()
  ;; TODO: * order will likely be redundant with the way of storing maps.
  ((order :documentation "grid order of the map."
          :accessor order :initarg :order)
   (peers-row-mat :documentation "Peers of pos in its row."
                  :accessor peers-row :initarg :peers-row)
   (peers-col-mat :documentation "Peers of pos in its col."
                  :accessor peers-col :initarg :peers-col)
   (peers-box-mat :documentation "Peers of pos in its box."
                  :accessor peers-box :initarg :peers-box)
   (peers-mat :documentation "Peers of pos in ALL units."
              :accessor peers :initarg :peers)))

(defun make-peers-row (n pos)
  "Make the list of peers in the row of pos."
  (let ((peers (list))
        (row (pos-row pos)))
    (loop for j from 0 below (* n n) do
      (when (/= (pos-col pos) j)
        (push (make-pos :row row :col j) peers)))
    peers))

(defun make-peers-col (n pos)
  "Make the list of peers in the column of pos."
  (let ((peers (list))
        (col (pos-col pos)))
    (loop for i from 0 below (* n n) do
      (when (/= (pos-row pos) i)
        (push (make-pos :row i :col col) peers)))
    peers))

(defun make-peers-box (n pos)
  "Make the list of peers in the box of pos."
  (let ((peers (list))
        (start-row (* n (truncate (pos-row pos) n)))
        (start-col (* n (truncate (pos-col pos) n))))
    (loop for i from start-row below (+ start-row n) do
      (loop for j from start-col below (+ start-col n) do
        (when (or (/= i (pos-row pos)) (/= j (pos-col pos)))
          (push (make-pos :row i :col j) peers))))
    peers))

(defun make-peers-all (n pos)
  "Make the list of all the peers for pos."
  (nunion (make-peers-row n pos)
          (nunion (make-peers-col n pos)
                  (make-peers-box n pos) :test #'equalp)
          :test #'equalp))

(defun make-map (n)
  "Instanciate a map for problems of order n."

  )
