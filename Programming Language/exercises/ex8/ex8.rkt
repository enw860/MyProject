#lang racket #| CSC324 Winter 2018: Exercise 8 |#
#|
★ Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html

Please note that we *are* exporting (and testing) some of the helper
functions we've described in this lab, and not just the main algorithms.
So please make sure to read and follow the specifications carefully for *all*
functions here!

Also, you may not change streams.rkt and stack_choice.rkt (you won't be able
to submit those files; we'll use our own for testing purposes).
★
|#
;-------------------------------------------------------------------------------
(provide solve-with-constraints
         with-constraints-helper
         get-constraints
         set->choice

         solve-with-ordered-constraints
         with-ordered-constraints-helper
         initialize-constraints
         sort-constraints
         update-constraints)


(require "streams.rkt")
(require "stack_choice.rkt")

;-------------------------------------------------------------------------------
; Sudoku Modeling
;-------------------------------------------------------------------------------

; We model a Sudoku board by a Racket *vector* of length 81, representing a 9x9 grid.
; (Rows are stored continuguously; for example, the top row of the board is stored in
; the first 9 elements of the vector.)
; Racket vectors are an array-based data structure, and provide constant-time
; indexing (unlike Racket lists).
;
; Each vector element is either a between between 1-9, representing a filled cell,
; or the number 0, representing an *empty* cell.
(define board-size 81)
(define 1-9 (list->set (range 1 10)))                      ; The possible numbers.
(define (blank? board i) (equal? (vector-ref board i) 0))  ; Whether the given cell is blank.


; Utilities for converting between a vector index and the corresponding row, column,
; and *3x3 subsquare* in the Sudoku board. This numbering is all 0-indexed.
; The subsquares are numbered starting with 0 in the top-left corner,
; and increase in index first left-to-right, then top-down.
(define (to-column i) (remainder i 9))
(define (to-row i) (quotient i 9))
(define (to-subsquare i)
  (+ (quotient (to-column i) 3)
     (* 3 (quotient (to-row i) 3))))

; Utilities for accessing the elements in a given column, row, and subsquare.
(define (column board i)
  (map (lambda (j) (vector-ref board (+ i (* 9 j)))) (range 9)))

(define (row board j)
  (map (lambda (i) (vector-ref board (+ i (* 9 j)))) (range 9)))

(define (subsquare board k)
  (let ([start-row (* 3 (quotient k 3))]
        [start-col (* 3 (remainder k 3))])
    (map (lambda (i)
           (vector-ref board
                       (+ (+ start-col (remainder i 3))
                          (* 9 (+ start-row (quotient i 3))))))
         (range 9))))


; Return whether a given Sudoku board is completely solved.
; (Review the rules of Sudoku using the link on the exercise handout.)
(define (solved? puzzle)
  (and
   ; Check columns
   (andmap (lambda (col-num) (set=? (list->set (column puzzle col-num)) 1-9))
           (range 9))
   ; Check rows
   (andmap (lambda (row-num) (set=? (list->set (row puzzle row-num)) 1-9))
           (range 9))
   ; Check subsquares
   (andmap (lambda (sub-num) (set=? (list->set (subsquare puzzle sub-num)) 1-9))
           (range 9))))


; Utility function for doing a non-mutating update of a board,
; analogous to list-set.
; This is pretty memory-inefficient, and is a consequence of some limitations
; of our current choice operator when mutation is concerned!
; We've provided an optional argument to turn on logging.
; This may be useful for debugging purposes, or to see how many steps your
; algorithm is taking.
(define (vector-set vec index new-item [logging #f])
  (when logging
    (displayln (format "Index: ~a Choice: ~a" index new-item)))

  (let ([new-vec (make-vector (vector-length vec))])
    (vector-copy! new-vec 0 vec)
    (vector-set! new-vec index new-item)
    new-vec))

(define (printBoard board i)
  (cond
    [(< i 9)
     (displayln (row board i))
     (printBoard board (+ 1 i))]
    [(= i 9) "done"]))

;-------------------------------------------------------------------------------
; A Brute Force Algorithm
;-------------------------------------------------------------------------------

; See `brute-force-helper` for details.
(define (solve-brute-force board) (brute-force-helper board 0))


#|
(brute-force-helper board i)
  puzzle: A Sudoku board, in the representation described above.
  i: The current index to consider.

  Considers each board cell one at a time (recurses on `i`).
  Each time it encounters an empty cell, this algorithm creates a *choice point*
  for all 9 numbers that could fill the cell.
  It chooses a number, sets it in the vector, and moves on to the next cell.

  Only when the board is complete does this algorithm check if the board is solved;
  if it isn't, it calls (fail) to backtrack to the last choice point, and tries again.
|#
(define (brute-force-helper board i)
  (cond
    ; If there are no more choices to make, check if the puzzle is actually solved.
    [(>= i (vector-length board))
     (if (solved? board)
         board
         (fail))]

    ; If the current cell is occupied, move on to the next cell.
    [(not (blank? board i))
     (brute-force-helper board (+ i 1))]

    ; Else, the current cell is empty. Make a new choice!
    [else
     (let* ([choice (-< 1 2 3 4 5 6 7 8 9)]
            [new-board (vector-set board i choice #t)])
       (brute-force-helper new-board (+ i 1)))]))

;-------------------------------------------------------------------------------
; Task 1: Narrowing choices using constraints
;-------------------------------------------------------------------------------

#|
`solve-with-constraints` (and its corresponding helper) are almost exactly the same
as `brute-force`. The only difference is in what choices are made; rather than
using a *static* set of choices, the choices for each cell are generated *dynamically*
based on the current contents of the board.

Complete the two helpers `get-constraints` and `set->choice`, and then modify
`with-constraints-helper` to replace the (-< 1 2 3 4 5 6 7 8 9) expression.
(You can change other things as well, although you shouldn't need to change much.)
|#
(define (solve-with-constraints board)
  (with-constraints-helper board 0))

(define (with-constraints-helper board i)
  (cond
    ; If there are no more choices to make, check if the puzzle is actually solved.
    [(>= i (vector-length board))
     (if (solved? board)
         board
         (fail))]

    ; If the current cell is occupied, move on to the next cell.
    [(not (blank? board i))
     (with-constraints-helper board (+ i 1))]

    ; Else, the current cell is empty. Make a new choice!
    [else
     (let* ([choices (get-constraints board i)]
            [choices-set (set->choice choices)]
            [new-board (vector-set board i choices-set #t)])
       (if (equal? choices-set (-<)) (fail)
           (with-constraints-helper new-board (+ i 1))))]))

#|
(get-constraints board i)
  board: A Sudoku board.
  i: An index of an *empty cell* in `board`.

  Returns a *set* of the possible numbers that can fill the empty cell.
  Starts with all 9 possible numbers, and removes the numbers that are
  in the same row, column or subsquare as the given cell.

  Note: You may assume we'll only test this function for the given constraint
  on `i`. In Task 2 you may find it useful to extend the documented behaviour
  for when `i` refers to an occupied cell in the board.
|#
(define (get-constraints puzzle i)
  (let ([cur-col (column puzzle (to-column i))]
        [cur-row (row puzzle (to-row i))]
        [cur-sqr (subsquare puzzle (to-subsquare i))]
        [prime-selection (range 1 10)])
    (list->set (foldl (lambda (val cul) (remove val cul))
           prime-selection (append cur-col cur-row cur-sqr)))))

#|
(set->choice set)
  set: A Racket set

  Returns a choice of an item in set, or calls `fail` if the set is empty.
  Hint: the set datatype has methods set-empty? set-first, and set-rest
  that are analogous to the list methods.
|#
(define (set->choice set)
  (if (set-empty? set) (-<)
      (let ([first-ele (set-first set)]
            [rest-ele (set-rest set)])
        (-< first-ele (set->choice rest-ele)))))

;-------------------------------------------------------------------------------
; Task 2: Greedily ordering choices
;-------------------------------------------------------------------------------

#|
`solve-with-ordered-constraints` builds on your work in the previous task by
tackling two limitations of the previous approach:

  1. The constraints for each cell are recomputed every time backtracking occurs.
  2. The naive index-order in which the cells are considered may delay applying
     stricter constraints on later cells, leading to more choices (and hence more
     backtracking) made for the early cells.

The main helpers you'll work on here are `initialize-constraints` and
`update-constraints`, which respectively create a list of constraints for all
cells at the start of solving the puzzle, and updating these constraints as
choices get made.

We've provided a helper `sort-constraints` for you that you should use to maintain
your list of constraints sorted by non-decreasing number of possibilities.
Your recursive helper will use this order to make choices, which should greatly reduce
the total number of choices made when solving most Sudoku boardsw.
|#
(define (solve-with-ordered-constraints board)
  (let ([constraints (sort-constraints (initialize-constraints board))])
    (with-ordered-constraints-helper board constraints)))

#|
(with-ordered-constraints-helper board constraints)
  board: A Sudoku board.
  constraints: A nested list of the constraints on the remaining blank cells,
               in the format described below in `initialize-constraints`.

  Precondition: `constraints` is sorted first by increasing size of the set of
  possible values, and then by increasing index.

  This is the main helper, analogous to the previous two algorithms.
  The main difference here is the second parameter; instead of using an index,
  we use a list of the remaining constraints explicitly.

  Hints:
    - Use the same basic structure as the previous algorithms,
      though the conditions will be different.
    - Attempt this *after* completing (and testing!) the two helpers below.
    - Remember the basic "first and rest" recursive pattern on lists, and use it here.
|#
(define (with-ordered-constraints-helper board constraints)
  (if (null? constraints)
      (if (solved? board)
          board
          (error "Cannot be solved"))
      (let* ([first-cons (first constraints)]
             [rest-cons (rest constraints)]
             [index (first first-cons)]
             [choices (second first-cons)]
             [choices-set (set->choice choices)])
        (if (equal? choices-set (-<)) (fail)
            (with-ordered-constraints-helper
                (vector-set board index choices-set #t)
              (update-constraints rest-cons index choices-set)))
        )))

#|
(initialize-constraints board)
  board: A Sudoku board.

  Returns a list of constraints for the blank cells in the given board.
  Represent each constraint as a list of two elements:
    - the first element is the index of the cell
    - the second element is a *set* containing the possible values
      that could fill the cell, using the same constraints as `get-constraints`.

  Assume that the board is solvable, which means that none of the blank cells
  will have an *empty* set of possible values.
|#
(define (initialize-constraints-helper board i)
  (cond
    [(>= i (vector-length board)) (list)]
    [(not (blank? board i)) (initialize-constraints-helper board (+ i 1))]
    [else (list* (list i (get-constraints board i))
                 (initialize-constraints-helper board (+ i 1)))]))
     
(define (initialize-constraints board)
  (initialize-constraints-helper board 0))

#|
(sort-constraints constraints)
  constraints: A nested list in the form described in `initialize-constraints`.

  Sorts the list of constraints first by increasing size of the set of possible values,
  and then by increasing index.

  This function is given to you; please don't change it!
|#
(define (sort-constraints constraints)
  (sort constraints
        (lambda (a b)
          (or (< (set-count (second a)) (set-count
                                         (second b)))
              (and (= (set-count (second a)) (set-count (second b)))
                   (< (first a) (first b)))))))

#|
(update-constraints constraints i choice)
  constraints: A nested list in the form described in `initialize-constraints`.
  i: A valid index into a Sudoku board.
  choice: A integer between 1-9.

  Updates the given constraint list by adding the restriction that cell `i` is
  being given value `choice`. That is, `choice` should be removed from all
  the "possible value" sets for the indexes in the same row, column, or subsquare
  as `i`.

  You may choose to re-sort the constraints here or in the main helper above.

  Important: we strongly recommend calling (fail) here if removing `choice`
  produces an empty set for one of the constraints. This corresponds to that
  cell no longer havin any possible values, meaning `choice` is incorrect.
  There are other places you could check for this, but it's probably easiest to
  do it here.
|#
(define (subsquare-index k)
  (let ([start-row (* 3 (quotient k 3))]
        [start-col (* 3 (remainder k 3))])
    (map (lambda (i)
           (+ (+ start-col (remainder i 3))
              (* 9 (+ start-row (quotient i 3)))))
         (range 9))))

(define (update-constraints constraints i choice)
  (let* ([col-index (to-column i)]
         [row-index (to-row i)]
         [subsq-index (to-subsquare i)]
         [numbers (range 9)]
         [related-index
          (sort (remove-duplicates
                 (append (map (lambda (x) (+ (* x 9) col-index)) numbers)
                         (map (lambda (x) (+ (* row-index 9) x)) numbers)
                         (subsquare-index subsq-index))) <)])
    (sort-constraints
     (map (lambda (x)
            (let* ([index (first x)]
                   [choices (second x)]
                   [rest-choice  (set-remove choices choice)])
              (if (memv index related-index)
                  (if (set-empty? rest-choice)
                      (fail)
                      (list index rest-choice))
                  (list index choices))))
          constraints)
     )))

;-------------------------------------------------------------------------------
; Demos
;-------------------------------------------------------------------------------
#|
This section includes some code for running your algorithms on actual Sudoku boards.

You can safely ignore all of this code, expect the invocations of the algorithms at
the bottom, which start off commented-out.

We took some puzzles from https://projecteuler.net/problem=96, but added our own
(very easy) puzzle at the front.
|#
(define in (open-input-file "p096_sudoku.txt"))


; A stream of puzzles.
(define (puzzle-stream)
  (let ([puzzle (get-next-puzzle)])
    (if (void? puzzle)
        s-null
        (s-cons puzzle (puzzle-stream)))))


; Get the next puzzle from the file.
; Note that this is written in an imperative style; as we'll discuss later
; in the course, it's much harder to get away from this style when doing I/O
; computations.
(define (get-next-puzzle)
  ; Check for the header line "Grid XX". If eof is found, we've reached the end of the file.
  (if (eof-object? (read-line in))
      (void)
      (let ([nested-list-form
             (map

              (lambda (_)
                ; This processes a single line, converting it from a 9-letter string into a list of integers.
                (map (lambda (c) (- (char->integer c) (char->integer #\0)))
                     (string->list (read-line in))))
              (range 9))])
        (list->vector (apply append nested-list-form)))))

(define all-puzzles (stream->list (puzzle-stream)))
(define easy (first all-puzzles))
(define harder (second all-puzzles))

;(solve-brute-force easy)
;(solve-with-constraints easy)

;(solve-with-constraints harder)
;(solve-with-ordered-constraints harder)

;Extra sudoku
(define easy1
  (vector
   4 0 0 5 0 0 7 0 0
   0 0 1 0 0 2 0 8 0
   0 0 0 0 0 7 9 0 0
   0 3 6 0 4 0 0 0 2
   0 0 0 2 0 0 0 0 0
   0 8 0 0 3 0 0 0 6
   0 0 0 9 0 8 5 0 0
   1 0 0 0 0 5 8 0 0
   3 0 0 6 0 0 0 1 0))

(define easy2
  (vector
   0 0 0 3 0 5 0 9 0
   0 5 7 4 0 0 0 0 8
   0 6 9 0 0 0 0 7 0
   0 7 8 6 0 0 9 5 3
   0 0 5 0 0 0 7 0 0
   9 1 3 0 0 2 8 6 0
   0 8 0 0 0 0 2 4 0
   4 0 0 0 0 7 5 3 0
   0 3 0 9 0 6 0 0 0))

(define mid1
  (vector
   0 0 7 0 0 5 0 0 9
   0 0 0 0 0 8 0 0 0
   2 0 5 7 3 0 0 0 0
   3 0 9 2 1 0 0 0 0
   0 0 0 0 0 0 0 0 4
   8 0 0 0 9 3 0 0 0
   0 7 0 0 0 0 3 0 0
   0 0 0 0 0 0 0 9 0
   0 0 2 0 8 0 6 0 1))

(define mid2
  (vector
   8 0 0 2 9 6 0 0 0
   0 0 0 7 0 0 0 0 5
   0 0 0 0 0 1 0 4 0
   1 9 0 0 0 0 2 0 0
   0 0 8 0 0 0 0 0 0
   0 6 0 0 0 0 7 0 0
   0 5 9 0 0 0 0 7 0
   0 0 0 6 0 3 1 5 0
   0 0 0 0 0 2 0 8 0))

(define hard1
  (vector
   0 0 0 0 0 4 7 0 0
   0 0 0 0 0 0 0 0 0
   3 0 7 5 0 0 0 8 9
   0 2 0 0 0 0 0 0 0
   5 0 8 0 0 7 0 0 0
   0 0 0 0 6 0 4 0 3
   0 0 0 3 0 0 1 0 2
   9 0 1 0 0 0 5 0 0
   0 0 5 6 0 0 0 4 0))

(define hard2
  (vector
   0 0 0 1 0 0 0 3 0
   7 0 4 0 0 0 5 0 0
   0 0 0 0 0 9 8 1 0
   0 0 0 3 8 0 0 0 0
   0 2 0 0 0 0 0 9 0
   1 0 0 6 0 7 0 0 0
   0 0 0 7 2 0 0 0 8
   9 0 0 0 0 3 1 0 0
   6 0 0 0 0 1 0 4 0))

(define blank-sudoku
  (vector
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0))

(module+ test
  (require rackunit)

  (test-true "brute force - Easy"
             (solved? (solve-brute-force easy)))
  
  (test-true "constraints - Easy"
             (solved? (solve-with-constraints easy)))

  (test-true "constraints - Harder"
             (solved? (solve-with-constraints harder)))

  (test-true "ordered-constraints - Easy"
             (solved? (solve-with-ordered-constraints easy)))

  (test-true "ordered-constraints - Harder"
             (solved? (solve-with-ordered-constraints harder)))
  
  (test-true "ordered-constraints - Easy1"
             (solved? (solve-with-ordered-constraints easy1)))
  
  (test-true "ordered-constraints - Easy2"
             (solved? (solve-with-ordered-constraints easy2)))
  
  (test-true "ordered-constraints - Mid1"
             (solved? (solve-with-ordered-constraints mid1)))
  
  (test-true "ordered-constraints - Mid2"
             (solved? (solve-with-ordered-constraints mid2)))
  
  (test-true "ordered-constraints - Hard1"
             (solved? (solve-with-ordered-constraints hard1)))
  
  (test-true "ordered-constraints - Hard2"
             (solved? (solve-with-ordered-constraints hard2)))

  (test-true "ordered-constraints - Blank-sudoku"
             (solved? (solve-with-ordered-constraints blank-sudoku))))