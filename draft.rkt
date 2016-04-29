;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname draft) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following line is REQUIRED (do not remove)
(require "puzlib.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Place your Personal Identification here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:


;; (transpose g) consumes a Grid and produces another grid where each column
;; becomes a row and vice versa
;; transpose: Grid -> Grid
;; requries: consumes Grid is a non empty grid
;; Examples:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))
(check-expect (transpose '((1 2 3)
                           (4 5 6)
                           (7 8 9)))
              '((1 4 7)
                (2 5 8)
                (3 6 9)))

(define (transpose g)
  (local [(define num-of-col (length (first g)))]
    (foldr
     (lambda (col z)
       (cons (foldr (lambda (x y) (cons (list-ref x col) y)) empty g) z))
     empty
     (build-list num-of-col (lambda (x) x)))))

;; Tests:
(check-expect (transpose (transpose grid-abc)) grid-abc)
(check-expect (transpose '((1))) '((1)))


;; (find-wpos loc row) consumes a list of Chars loc that corresponds to a row in
;; a grid and the row number and produces a list of all the word positions with
;; more than one character that are possible in that row 
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))
(check-expect (find-wpos (string->list "#.#.#.#") 0) empty)

(define (find-wpos loc row)
  (local
    [;; (first-word loc) consumes a list of characters and produces the list of
     ;; the first continuous squence of empty cells, if the loc starts with an
     ;; unused cell it will produce empty
     ;; first-word: (listof Char) -> (listof Char)
     (define (first-word loc)
       (cond [(empty? loc) empty]
             [(char=? (first loc) unused-cell) empty]
             [else (cons (first loc) (first-word (rest loc)))]))

     ;; (rest-words loc) removes the sequence of continous empty cells in the
     ;; begining of the loc and produces the rest
     ;; rest-words: (listof Char) -> (listof Char)
     (define (rest-words loc)
       (cond [(empty? loc) empty]
             [(char=? (first loc) unused-cell) loc]
             [else (rest-words (rest loc))]))

     ;; (next-wpos loc col) consumes a list of characters and and column number
     ;; from which the list of characters start its index and produces all
     ;; the possible wpos in that list so that col is the start of the index for
     ;; the loc
     ;; next-wpos: (listof Char) Nat -> (listof Wpos)
     (define (next-wpos loc col)
       (local [(define first-word-length (length (first-word loc)))]
         (cond [(empty? loc) empty]
               [(char=? unused-cell (first loc))
                (next-wpos (rest loc) (add1 col))]
               [else (cons (make-wpos row
                                      col
                                      true
                                      first-word-length)
                           (next-wpos
                            (rest-words loc) (+ col first-word-length)))])))]
    (filter (lambda (x) (> (wpos-len x) 1)) (next-wpos loc 0))))

;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
;; the order does not matter: here is an example
;; that uses lists-equiv?
(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)


;; (initial-state puzzle) consumes a puzzle (puzzle) and produces the state of
;; the puzzle with any of the words being placed
;; initial-state: Puzzle -> State
;;Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))

(define (initial-state puzzle)
  (local [;; (los->grid los) consumes a list of strings (los) and produces a
          ;; grid (listof (listof Char))
          ;; los->grid: (listof String) -> (listof (listof Char))
          ;; requries: all the strings in los must be of the same length
          (define (los->grid los)
            (cond [(empty? los) empty]
                  [else (cons (string->list (first los))
                              (los->grid (rest los)))]))
          
          (define (find-hwpos-grid g n)
            (cond [(empty? g) empty]
                  [else (append (find-wpos (first g) n)
                           (find-hwpos-grid (rest g) (add1 n)))]))

          (define (find-vwpos-grid g)
                 (map flip (find-hwpos-grid (transpose g) 0)))
          ]
    (make-state (los->grid (first puzzle))
                (append
                 (find-hwpos-grid (los->grid (first puzzle)) 0)
                 (find-vwpos-grid (los->grid (first puzzle))))
                (second puzzle))))

;; Tests:
(check-expect (initial-state (read-puzzle "puzzle02.txt"))
              (make-state
               (list
                (list #\. #\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
                (list #\. #\. #\. #\. #\. #\. #\# #\# #\# #\# #\.)
                (list #\. #\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
                (list #\. #\. #\. #\. #\. #\# #\# #\# #\# #\# #\#)
                (list #\. #\. #\. #\. #\. #\. #\# #\. #\# #\. #\.)
                (list #\. #\. #\. #\. #\. #\. #\# #\. #\. #\. #\.)
                (list #\. #\. #\. #\. #\. #\. #\# #\. #\. #\. #\.)
                (list #\. #\. #\. #\# #\# #\# #\# #\. #\. #\. #\.)
                (list #\. #\. #\. #\# #\. #\. #\# #\. #\. #\. #\.)
                (list #\# #\# #\# #\# #\. #\. #\. #\. #\. #\. #\.))
               (list
                (make-wpos 1 6 true 4)
                (make-wpos 3 5 true 6)
                (make-wpos 7 3 true 4)
                (make-wpos 9 0 true 4)
                (make-wpos 7 3 false 3)
                (make-wpos 3 6 false 6)
                (make-wpos 0 8 false 5))
               (list "ADAM" "ALBERT" "DAN" "DAVE" "JOHN" "KAREN" "LESLEY")))
(check-expect (initial-state (read-puzzle "puzzle03.txt"))
              (make-state
               (list
                (list #\# #\# #\# #\# #\#)
                (list #\# #\# #\# #\# #\#)
                (list #\# #\# #\# #\# #\#)
                (list #\# #\# #\# #\# #\#)
                (list #\# #\# #\# #\# #\#))
               (list
                (make-wpos 0 0 true 5)
                (make-wpos 1 0 true 5)
                (make-wpos 2 0 true 5)
                (make-wpos 3 0 true 5)
                (make-wpos 4 0 true 5)
                (make-wpos 0 0 false 5)
                (make-wpos 0 1 false 5)
                (make-wpos 0 2 false 5)
                (make-wpos 0 3 false 5)
                (make-wpos 0 4 false 5))
               (list "SATOR" "AREPO" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO"
                     "TENET" "OPERA" "ROTAS")))

;; (extract-wpos g wp) consumes a Grid g and a Wpos and returns the list of
;; Chars correspondong the the Wpos consumed
;; extract-wpos: Grid WPos -> (listof Char)
;; Examples: 
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))

(define (extract-wpos g wp)
  (local
    [(define myloc (cond [(wpos-horiz? wp) (list-ref g (wpos-row wp))]
                         [else (list-ref (transpose g) (wpos-col wp))]))
     (define ref (cond [(wpos-horiz? wp) (wpos-col wp)]
                       [else (wpos-row wp)]))
     (define len (wpos-len wp))]
    (foldr (lambda (x y) (cons (list-ref myloc x) y))
           empty
           (build-list len (lambda (x) (+ ref x))))))

;; Tests:
(check-expect (extract-wpos '((1 2 3)
                              (4 5 6)
                              (7 8 9)) (make-wpos 0 0 true 3)) '(1 2 3))
(check-expect (extract-wpos '((1 2 3)
                              (4 5 6)
                              (7 8 9)) (make-wpos 0 0 false 3)) '(1 4 7))

;; (replace-wpos g wp loc) consumes a grid g, a word positon wp and list of
;; characters loc and replace the word position wp in the grid g with the
;; list of characters loc
;; replace-wpos: Grid WPos (listof Char) -> Grid
;; requires: len in WPos is equal to length of (listof Char)
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc)
  (local
    [;; (first-k alst k) consumes a list (alst) and produces the first k terms
     ;; in this list
     ;; first-k: (listof Any) Nat -> (listof Any)
     (define (first-k alst k)
       (cond [(zero? k) empty]
             [(empty? alst) empty]
             [else (cons (first alst)
                         (first-k (rest alst) (sub1 k)))]))
     
     ;; (remove-k alst k) consumes a list (alst) and removes the first k terms
     ;; in that list and prodcues a new list without those first k terms
     ;; remove-k: (listof Any) Nat -> (listof Any)
     (define (remove-k alst k)
       (cond [(zero? k) alst]
             [else (remove-k (rest alst) (sub1 k))]))
     
     ;; (replace-wpos-in-row row wpos loc) consumes a row (row) and a word
     ;; position in that row and list of characters and replaces the wpos in the
     ;; row with the list of characters
     ;; replace-wpos-in-row: (listof Char) Wpos (listof Char) -> (listof Char)
     ;; requries: the length of the loc consumes must be equal to the length
     ;; of the wpos
     (define (replace-wpos-in-row row wpos loc)
       (append (first-k row (wpos-col wpos))
               loc
               (remove-k row (+ (wpos-col wpos)
                                (wpos-len wpos)))))
     
     ;; (replace-wpos-in-grid grid wpos loc) consumes a Grid (grid) and Wpos
     ;; (wpos) and a list of characters (loc) and replaces the wpos in the
     ;; grid with the loc
     ;; replace-wpos-in-grid: Grid Wpos (listof Char) -> Grid
     ;; requries: the loc and wpos must have the same length
     (define (replace-wpos-in-grid grid wpos loc)
       (cond [(empty? grid) empty]
             [(= 0 (wpos-row wpos))
              (cons (replace-wpos-in-row (first grid)
                                         wpos
                                         loc)
                    (rest grid))]
             [else (cons (first grid)
                         (replace-wpos-in-grid
                          (rest grid)
                          (make-wpos (sub1 (wpos-row wpos))
                                     (wpos-col wpos)
                                     (wpos-horiz? wpos)
                                     (wpos-len wpos))
                          loc))]))]
    (cond [(wpos-horiz? wp)
           (replace-wpos-in-grid g wp loc)]
          [else
           (transpose
            (replace-wpos-in-grid
             (transpose g)
             (flip wp)
             loc))])))

;; Tests:
(check-expect (replace-wpos '((1 2 3)
                              (4 5 6)
                              (7 8 9)) (make-wpos 0 0 false 3) '(1 2 3))
              '((1 2 3)
                (2 5 6)
                (3 8 9)))
(check-expect (replace-wpos '((1 2 3)
                              (4 5 6)
                              (7 8 9)) (make-wpos 0 0 true 3) '(1 4 7))
              '((1 4 7)
                (4 5 6)
                (7 8 9)))

;; (fit? word cells) consumes a list of characters that represent a word (word)
;; and a list of chars that represent a group of cells in a grid (cells) and
;; determines if the word consumed would fit the group of cells consumed or not
;; fit? (listof Char) (listof Char) -> Bool
;; Examples:
(check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)

(define (fit? word cells)
  (cond [(not (= (length word) (length cells))) false]
        [(empty? word) true]
        [(char=? (first word) (first cells)) (fit? (rest word) (rest cells))]
        [(char=? (first cells) #\#) (fit? (rest word) (rest cells))]
        [else false]))

;; Tests:
(check-expect (fit? (string->list "HAMMER") (string->list "#####")) false)
(check-expect (fit? (string->list "HAMMER") (string->list "######")) true)

;; (neighbours s)
;; neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(define (neighbours s)
  (local
    [;; (sort-wpos grid list-of-wp) consumes grid grid and a list of wpos which
     ;; are defined with respect to the grid consumed and it produces a sorted
     ;; list in a descending order of the wpos in terms of the characters filled
     ;; in each wpos
     ;; sort-wpos: Grid (listof Wpos) -> (listof Wpos)
     (define (sort-wpos grid list-of-wp)
       (quicksort list-of-wp
                  (lambda (x y) (> (count-chars (extract-wpos grid x))
                                   (count-chars (extract-wpos grid y))))))
     
     ;; (count-chars loc) consumes a list of characters and counts the number of
     ;; characters which are not empty (#) characters
     ;; count-chars: (listof Char) -> Nat
     (define (count-chars loc)
       (cond [(empty? loc) 0]
             [(char=? (first loc) #\#) (count-chars (rest loc))]
             [else (add1 (count-chars (rest loc)))]))
     
     (define (neighbours-words state low)
       (local [(define best-wpos (first (sort-wpos (state-grid state)
                                                   (state-positions state))))
               (define best-wpos-loc
                 (extract-wpos (state-grid state) best-wpos))]
         (cond
           [(empty? low) empty]
           [(fit? (string->list (first low)) best-wpos-loc)
                (cons (make-state (replace-wpos (state-grid state)
                                                best-wpos
                                                (string->list (first low)))
                                  (remove best-wpos (state-positions state))
                                  (remove (first low) (state-words state)))
                      (neighbours-words state (rest low)))]
               [else (neighbours-words state (rest low))])))]
    (neighbours-words s (state-words s))))

;; Tests:
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

(check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window
