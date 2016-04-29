(module a10 scheme
  (require racket/gui/base)
  (require racket/draw)
  
  ;; ************************ SETABLE CONSTANTS ***************************
  
  ;; drawing speed (anyof 'norm 'fast 'slow 'custom 'off)
  (define draw-option 'norm)
  
  ;; define the number of second for the custom speed to wait between steps
  (define custom-speed 1.5)
  
  ;; the thickness of the square borders
  (define font-exp (make-object font% 5 'default))
  
  ;; the size of the letters in the solution
  (define font-val (make-object font% 65 'default))
  
  ;; the size of each square
  (define square-size 20)
  
  ;; horizontal offset of each letter
  (define horz-offset 2)
  
  ;; vertical offset of each letter
  (define vert-offset 6)
  
  
  ;; ********** STUDENTS DO NOT NEED TO MODIFY BELOW THIS LINE ************
  
  ;; name of the puzzle
  (define puzz-name "Solve-CrissCross")
  
  ;; predicate for blank cells in the grid
  (define (blank? x)
    (char=? #\# x))
  
  ;; predicate for out of bounds cells in the grid
  (define (oob? x)
    (char=? #\. x))
  
  ;; predicate for filled-in values in the grid
  (define (filled-in? x)
    (not (or (oob? x)
             (blank? x))))
  
  ;; selector of the grid to be drawn from a state of the puzzle
  (define (get-grid state)
    (vector-ref (struct->vector state) 1))
  
  ;; get the width of the state's grid
  (define (get-width state)
    (cond
      [(empty? false)]
      [else
       (length (first (get-grid state)))]))
  
  ;; get the height of the state's grid
  (define (get-height state)
    (length (get-grid state)))
  
  ;; base grid to be solved with nothing filled in (needed for KenKen)
  ;; (define blank-grid false)
  
  (define grid-frame (new frame% [label puzz-name]
                          [width (* 8 square-size)]
                          [height (+ 22 (* 8 square-size))]
                          [x 50]
                          [y 50]))
  
  (define grid-canvas (new canvas% [parent grid-frame]))
  
  (define grid-dc (send grid-canvas get-dc))
  (send grid-dc set-font font-val)
  
  ;; resize the frame when solving a new puzzle
  (define (resize-frame new-width new-height)
    (begin
      (show-grid false)
      (set! grid-frame (new frame%
                            [label puzz-name]
                            [height (+ 22 (* 2 new-height square-size))]
                            [width (* 2 new-width square-size)]
                            [x 50]
                            [y 50]))
      (set! grid-canvas (new canvas% [parent grid-frame]))
      (set! grid-dc (send grid-canvas get-dc))
      (send grid-dc set-scale 2 2)
      (show-grid true)))
  
  ;; Some initial set up before a puzzle can be drawn
  (define (puzzle-setup puzz)
    (cond
      [(not (symbol=? draw-option 'off))
       (begin
         ;(set! blank-grid (get-grid puzz))
         (resize-frame (get-width puzz) (get-height puzz)))]))
  
  ;; Make the board viewable
  (define (show-grid bool)
    (send grid-frame show bool))
  
  
  ;; ************************* DRAW FUNCTIONS *****************************
  
  ;; fill in a cell that is out of bounds
  (define (draw-oob x-coord y-coord)
    (define cur-brush (send grid-dc get-brush))
    (send grid-dc set-brush "black" 'solid)
    (send grid-dc draw-rectangle (* square-size y-coord) (* square-size x-coord)
          square-size square-size)
    (send grid-dc set-brush cur-brush))
  
  ;; fill in an empty cell
  (define (draw-blank x-coord y-coord)
    (send grid-dc draw-rectangle (* square-size y-coord) (* square-size x-coord)
          square-size square-size))
  
  ;; fill in a cell with filled-in values
  (define (draw-filled-in cur-grid x-coord y-coord)
    (send grid-dc draw-text (list->string (list (list-ref (list-ref cur-grid x-coord) y-coord)))
          (+ vert-offset (* square-size y-coord))
          (+ horz-offset (* square-size x-coord))))
  
  ;; determine how a cell should be filled in
  (define (draw-cell cur-grid x-coord y-coord)
    (draw-blank x-coord y-coord)
    (cond
      [(filled-in? (list-ref (list-ref cur-grid x-coord) y-coord))
       (draw-filled-in cur-grid x-coord y-coord)]
      [(oob? (list-ref (list-ref cur-grid x-coord) y-coord))
       (draw-oob x-coord y-coord)]
      [(blank? (list-ref (list-ref cur-grid x-coord) y-coord))
       (draw-blank x-coord y-coord)]
      [else (error "unspecified tile")]))
  
  
  ;; draw all cells
  (define (draw-grid state)
    (cond
      [(not (symbol=? draw-option 'off))
       (build-list (get-width state)
                   (lambda (row) (build-list (get-height state)
                                             (lambda (col) (draw-cell (get-grid state)
                                                                      col row)))))
       (cond
         [(symbol=? 'slow draw-option) (sleep 0.75)]
         [(symbol=? 'fast draw-option) (sleep 0.05)]
         [(symbol=? 'custom draw-option) (sleep custom-speed)]
         [else (sleep 0.2)])]))
  
  
  ;; ************************* END OF GRAPHICS ****************************
  
  
  
  ;; puzlib :: A module to provide useful helper functions for
  ;;           CS 135, Fall 2015, Assignment 10
  ;;           by Dave Tompkins [dtompkins]
  
  (provide read-puzzle solve disp lists-equiv? unused-cell empty-cell)
  ;; read-puzzle: read in a puzzle file
  ;; solve: performs the main search
  ;; disp: display a list of strings in the interactions window
  ;; lists-equiv?: useful for testing
  ;; blank-char, word-char: constants for grids & puzzle files
  
  
  ;; constants for grids & puzzle files:
  (define comment-char #\;)
  (define unused-cell #\.)
  (define empty-cell #\#)
  
  
  
  ;; (grid-char? c) determines if c is a valid grid character
  ;; grid-char: Char -> Bool
  
  (define (grid-char? c)
    (or (char=? c unused-cell) (char=? c empty-cell)))
  
  
  ;; (next-line stream type) reads in a line from the file stream,
  ;;   verifying the line type ('numeric, 'grid, 'word)
  ;;   ignoring any lines starting with a semi-colon (;)
  ;; next-line: 'FileStream' Sym -> Str
  
  (define (next-line stream type)
    (local [(define line (read-line stream))]
      (cond [(eof-object? line)
             (error "error: reached end of file")]
            [(char=? (string-ref line 0) comment-char) (next-line stream type)]
            [(and (symbol=? type 'numeric)
                  (not (string->number line)))
             (error "error: invalid numeric input in file")]
            [(and (symbol=? type 'grid)
                  (not (andmap grid-char? (string->list line))))
             (error "error: invalid grid characters in file")]
            [(and (symbol=? type 'word)
                  (not (andmap char-alphabetic? (string->list line))))
             (error "error: invalid word characters in file")]          
            [else line])))
  
  
  ;; (read-puzzle filename) reads in the puzzle contained in filename
  ;;   and produces a list with two elements:
  ;;   * the puzzle grid (a listof str) [with only .'s and #'s]
  ;;   * the list of words
  ;; read-puzzle Str -> (list (listof Str) (listof Str))
  
  (define (read-puzzle filename)
    (local [(define in (open-input-file filename))
            (define numlines (string->number (next-line in 'numeric)))
            (define grid (build-list numlines (lambda (n) (next-line in 'grid))))
            (define numwords (string->number (next-line in 'numeric)))
            (define words (build-list numwords (lambda (n) (next-line in 'words))))]
      (list grid words)))
  
  
  ;; Note that in the following contracts, X is your State
  
  ;; (solve initial-state neighbours solved?) solves the problem by searching
  ;;   from an initial-state until a solution is found (solved? is true)
  ;;   or false if no solution exists.
  ;;   neighbours produces a list of legal next states from a given state.
  ;; find-route: X (X -> (listof X)) (X -> Bool) -> (anyof false X)
  
  (define (solve initial-state neighbours solved?)
    (with-handlers ([(lambda(err) true) (lambda (err) (send grid-frame on-exit) false)])
      (puzzle-setup initial-state)
      (define result (local
                       [;; (solve/single state) searches outward from state
                        ;;   looking for a solution
                        ;; solve/single: X -> (anyof false X)
                        (define (solve/single state)
                          (cond [(solved? state) state]
                                [else (solve/list (neighbours state))]))
                        
                        ;; (solve/list lostate) searches from every state in lostate
                        ;;   looking for a solution
                        ;; solve/list: (listof X) -> (anyof false X)
                        (define (solve/list lostate)
                          (cond [(empty? lostate) false]
                                [else
                                 (local [(define draw (draw-grid (first lostate)))
                                         (define fresult (solve/single (first lostate)))]
                                   (cond [(false? fresult) (solve/list (rest lostate))]
                                         [else fresult]))]))]
                       (solve/single initial-state)))
      (cond
        [(equal? draw-option 'off) result]
        [else
         (display "Finished.\n")
         (display "Press the eof button to print the result.\n")
         (flush-output (current-output-port))
         (read-char)
         (send grid-frame on-exit)
         result])))
  
  
  ;; (disp los) displays a list of strings los in the interactions window
  ;;   replacing periods (.) with spaces
  ;; disp: (listof Str) -> Void
  ;; effects: displays result in the interactions window
  
  (define (disp los)
    (local [(define los/lf (map (lambda (s) (string-append s "\n")) los))
            (define loc (apply append (map string->list los/lf)))
            (define clean (map (lambda (c) (cond [(char=? unused-cell c) #\space]
                                            [else c])) loc))]
      (display (list->string clean))))
  
  
  ;; lists-equiv? is not really part of this assignment,
  ;; but generally useful in tests where we don't care about ordering.
  ;; The approach is a bit sneaky, but very succinct: Check that
  ;; every element of l1 appears somewhere in l2 (in terms of equal?),
  ;; and that every elements of l2 appears somewhere in l1.
  
  ;; (lists-equiv? l1 l2) determines whether the l1 and l2 are "equivalent"
  ;;   and are essentially the same up to reordering.
  ;; lists-equiv?: (listof Any) (listof Any) -> Bool
  ;; requires: elements of l1 (or l2) are unique (no duplicates)
  
  (define (lists-equiv? l1 l2)
    (and (= (length l1) (length l2))
         (andmap (lambda (x1) (ormap (lambda (x2) (equal? x1 x2)) l2)) l1)
         (andmap (lambda (x2) (ormap (lambda (x1) (equal? x1 x2)) l1)) l2))))
