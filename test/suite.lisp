(defpackage :sdku-rep/test
  (:documentation "Rove test suite for sdku-rep.")
  (:use :cl :rove))

(in-package :sdku-rep/test)

(deftest sdku-rep
  (let ((n 3)
        (grid-a))
    (testing "Instantiating a grid."
      (setf grid-a (sdku-rep:make-grid n))
      (ok grid-a ""))
    (testing "Setting and reading cell."
      (setf (sdku-rep:cell grid-a (sdku-rep:pos 2 4)) 5)
      (ok (= 5 (sdku-rep:cell grid-a (sdku-rep:pos 2 4))) ""))
    (testing "Reading order of grid."
      (ok (= n (sdku-rep:order grid-a)) ""))
    ;; TODO: * Test that values are unique.
    (testing "Reading available problem values"
      (ok (= (length (sdku-rep:vals grid-a)) (* n n)) ""))
    (testing "Row peers of a cell"
      (ok (= (length (sdku-rep:peers-row grid-a (sdku-rep:pos 2 3)))
             (1- (* n n))) ""))
    (testing "Column peers of a cell"
      (ok (= (length (sdku-rep:peers-col grid-a (sdku-rep:pos 2 3)))
             (1- (* n n))) ""))
    (testing "Box peers of a cell"
      (ok (= (length (sdku-rep:peers-box grid-a (sdku-rep:pos 2 3)))
             (1- (* n n))) ""))
    (testing "All peers of a cell"
      (ok (= (length (sdku-rep:peers grid-a (sdku-rep:pos 2 3)))
             (- (* 3 (1- (* n n))) (* 2 (- n 1)))) ""))))
