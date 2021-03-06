(in-package :sdku-rep)

(defstruct pos
  (row 0 :type fixnum)
  (col 0 :type fixnum))

(defun pos (i j)
  (make-pos :row i :col j))

(defun posref (mat pos)
  "custom aref for pos."
  (aref mat (pos-row pos) (pos-col pos)))

(defun (setf posref) (new-val mat pos)
  "setf a mat at pos."
  (setf (aref mat (pos-row pos) (pos-col pos)) new-val))
