;;;; Map holds the general logical structure of problems: which pos are peers
;;;; of a given pos in a given unit.
(in-package :sdku-rep)

;; Note:
;; * Peers are the other pos of a given pos in its unit
;; (row, col, box or all of them).
;; * A pos is not its own peer.
;; * A map is true for all grid instances of a given order.
(defclass sdku-map ()
  ((order :documentation "grid order of the map."
          :accessor order :initarg :order)
   (vals :documentation "Set of allowable values in the problem.
                         eg. 1 to 9 for n=3."
         :accessor vals :initarg :vals)
   (peers-row-mat :documentation "Peers of pos in its row."
                  :accessor peers-row-mat :initarg :peers-row-mat)
   (peers-col-mat :documentation "Peers of pos in its col."
                  :accessor peers-col-mat :initarg :peers-col-mat)
   (peers-box-mat :documentation "Peers of pos in its box."
                  :accessor peers-box-mat :initarg :peers-box-mat)
   (peers-all-mat :documentation "Peers of pos in ALL units."
                  :accessor peers-all-mat :initarg :peers-all-mat)))

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

(defun make-vals (n)
  "Create the list of allowable values in the problem."
  (loop for i from 1 upto (* n n) collect i))

(defun make-map (n)
  "Instantiate a map for problems of order n."
  (let ((peers-row-mat (make-array (list (* n n) (* n n))))
        (peers-col-mat (make-array (list (* n n) (* n n))))
        (peers-box-mat (make-array (list (* n n) (* n n))))
        (peers-all-mat (make-array (list (* n n) (* n n))))
        (pos-cur (make-pos)))
    (loop for i from 0 below (* n n) do
      (loop for j from 0 below (* n n) do
        (psetf (pos-row pos-cur) i (pos-col pos-cur) j)
        (psetf (aref peers-row-mat i j) (make-peers-row n pos-cur)
               (aref peers-col-mat i j) (make-peers-col n pos-cur)
               (aref peers-box-mat i j) (make-peers-box n pos-cur)
               (aref peers-all-mat i j) (make-peers-all n pos-cur))))
    (make-instance
     'sdku-map :order n
     :peers-row-mat peers-row-mat
     :peers-col-mat peers-col-mat
     :peers-box-mat peers-box-mat
     :peers-all-mat peers-all-mat
     :vals (make-vals n))))

;;; Global map for each order.
(defvar *maps* (make-hash-table))

(defun make-glob-map (n)
  "Create a global map for order n if it wasn't already created."
  (when (not (nth-value 1 (gethash n *maps*)))
    (setf (gethash n *maps*) (make-map n)))
  (gethash n *maps*))

;; TODO: free map function
