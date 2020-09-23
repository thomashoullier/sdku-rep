# Sudoku representation
The system `sdku-rep` contains useful abstractions for dealing with
Sudoku problems of order n.

A problem instance is expressed with a `grid` object, which holds:
* An addressable **grid** of `cell`s, indexed with `pos` (row and columns).
* A reference to a **map** of the problem structure, which holds the relation
  between all `cell`s.

Following Peter Norvig [1] (and no doubt many other authors), we structure
the problem of sudoku as follows:
* A grid has *order* `n`. It contains $n^2 \times n^2$ *cells*.
* There are $n^2$ unique *values* in the problem (typically numbers
  from 1 to $n^2$ but can be any set of "distinguishable" symbols).
* A single cell is in three *units* (which are overlapping sets in the grid):
  *row*, *column*, and *box*.
* For a given cell, we call the other cells belonging to the same unit
  its *peers*. A cell has:
  * $n^2 - 1$ peers in a row.
  * $n^2 - 1$ peers in a column.
  * $n^2 - 1$ peers in a box.
  * $3 \times (n^2 - 1) - 2 \times (n - 1)$ distinct peers in total.
* The problem is solved if the cells of each unit are filled with a permutation
  of the available problem values set.

Formulated this way, the physical Sudoku grid can be seen as merely a
map indicative of how the sets of cells overlap. (It would be
interesting to see what the generalization of sudoku problems to other
overlapping schemes looks like. If you know what mathematical object
that is and can point to a paper, please let me know).

## Exported functions
**make-grid** n => grid-instance

Instantiate a grid of order `n`. The cells are filled with 0 by default.

* **pos** i j => pos

Create a *pos* structure, indexing the `grid` in rows *i* and columns *j*,
counting from 0.

* **cell** grid pos => value

Return cell value in *grid* at *pos*. Cells are also `setf`able.

* **order** grid => n

Return the order *n* of the *grid*.

* **vals** grid => problem-values

Return the list of available distinct symbols for the *grid* problem.

* **peers-row** grid pos => peers-pos

Return the list of positions `pos` corresponding to the row peers of the
cell at *pos*.

* **peers-col** grid pos => peers-pos
* **peers-box** grid pos => peers-pos

Idem for columns and boxes respectively.

* **peers** grid pos => peers-pos

Idem for all distinct peers across all units.

## Usage
Instantiate a grid.

```common-lisp
(defparameter *grid3* (make-grid 3))
```

Accessing the cell value at row 2, column 4.

```common-lisp
(setf (cell *grid3* (pos 2 4)) 5)
(cell *grid3* (pos 2 4)) ; => 5
```

Reading the order of the grid instance.

```common-lisp
(order *grid3*) ; => 3
```

Reading the available symbols for the problem.

```common-lisp
(vals *grid3*) ; => (1 2 3 4 5 6 7 8 9)
```

Lists of peers.

```common-lisp
(peers-row *grid3* (pos 2 4))
;; =>
;; (#S(POS :ROW 2 :COL 8) #S(POS :ROW 2 :COL 7)
;;  #S(POS :ROW 2 :COL 6) #S(POS :ROW 2 :COL 5)
;;  #S(POS :ROW 2 :COL 3) #S(POS :ROW 2 :COL 2)
;;  #S(POS :ROW 2 :COL 1) #S(POS :ROW 2 :COL 0))

(peers-col *grid3* (pos 2 4))
;; =>
;; (#S(POS :ROW 8 :COL 4) #S(POS :ROW 7 :COL 4)
;;  #S(POS :ROW 6 :COL 4) #S(POS :ROW 5 :COL 4)
;;  #S(POS :ROW 4 :COL 4) #S(POS :ROW 3 :COL 4)
;;  #S(POS :ROW 1 :COL 4) #S(POS :ROW 0 :COL 4))

(peers-box *grid3* (pos 2 4))
;; =>
;; (#S(POS :ROW 2 :COL 5) #S(POS :ROW 2 :COL 3)
;;  #S(POS :ROW 1 :COL 5) #S(POS :ROW 1 :COL 4)
;;  #S(POS :ROW 1 :COL 3) #S(POS :ROW 0 :COL 5)
;;  #S(POS :ROW 0 :COL 4) #S(POS :ROW 0 :COL 3))

(peers *grid3* (pos 2 4))
;; =>
;; (#S(POS :ROW 0 :COL 3) #S(POS :ROW 0 :COL 4)
;;  #S(POS :ROW 0 :COL 5) #S(POS :ROW 1 :COL 3)
;;  #S(POS :ROW 1 :COL 4) #S(POS :ROW 1 :COL 5)
;;  #S(POS :ROW 8 :COL 4) #S(POS :ROW 7 :COL 4)
;;  #S(POS :ROW 6 :COL 4) #S(POS :ROW 5 :COL 4)
;;  #S(POS :ROW 4 :COL 4) #S(POS :ROW 3 :COL 4)
;;  #S(POS :ROW 2 :COL 8) #S(POS :ROW 2 :COL 7)
;;  #S(POS :ROW 2 :COL 6) #S(POS :ROW 2 :COL 5)
;;  #S(POS :ROW 2 :COL 3) #S(POS :ROW 2 :COL 2)
;;  #S(POS :ROW 2 :COL 1) #S(POS :ROW 2 :COL 0))
```

## Test
Launch tests with:

```common-lisp
(asdf:test-system "sdku-rep")
```

## Dependencies
* `sdku-rep`: none.
* `sdku-rep/test`:
  * [rove](https://github.com/fukamachi/rove)

## References
1. http://norvig.com/sudoku.html
