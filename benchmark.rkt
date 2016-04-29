(module benchmark (lib "plt-pretty-big-text.ss" "lang")
  
  ;; Owen Smith and Josh Mackenzie Fall of 2010
  ;; Originally for CS135 assignment 10, Crossword Puzzle
  
  ;; If you Intend to reuse this module you must tailor the
  ;; benchmark test in procedure "bench-proc" to stress test
  ;; in a similar manor to the way the program you are 
  ;; benchmarking stresses the system, i.e if it is a memory
  ;; intensive program, you should stress test the user's
  ;; memory speed in "bench-proc".
  
  ;; On a side note, though this should be obvious, all
  ;; reference data (directly below this under header
  ;; "Reference Data" should be changed to the results of
  ;; the new "benchmark" machine under the new "bench-proc"
  ;; if these values are not changed, you may not get an 
  ;; accurate comparison
  
  ;; Module provides benchmarking tools for any function.
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Reference Data
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define ref-cpu 52319.4)
  (define ref-real 52298.1)
  (define ref-gc 18959.9)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Display Help
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (help)
    (printf "help: this help screen\n\tcontract: Void -> Void\n\t   usage: (help)\n\n")
    (printf "print-score: a scoring of your computer for use with benchmark-with-score\n\tcontract: Void -> Void\n\t   usage: (print-score)\n\n")
    (printf "benchmark: runs the benchmark against a passed in process, computes score at runtime\n\tcontract: (X1 X2 ... -> Any) [X1 X2 ...] -> Void\n\t   usage: (benchmark criss-cross (read-puzzle \"puzzle04.txt\"))\n\n")
    (printf "benchmark-with-score: runs the benchmark against a passed in process and arguments,\n\t\t      uses a given score rather than computing it at runtime\n\tcontract: Nat (X1 X2 ... -> Any) [X1 X2 ...] -> Void\n\t   usage: (benchmark-with-score 34064717 criss-cross (read-puzzle \"puzzle04.txt\"))\n\n"))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Functions to retrieve various run-time values of a function
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (get-time proc args)
    (collect-garbage)
    (let-values ([(val cpu real gc) (time-apply proc args)])
      '(cpu real gc)))
  
  (define (get-time-cpu proc args)
    (collect-garbage)
    (let-values ([(val cpu real gc) (time-apply proc args)])
      cpu))
  
  (define (get-time-real proc args)
    (collect-garbage)
    (let-values ([(val cpu real gc) (time-apply proc args)])
      real))
  
  (define (get-time-gc proc args)
    (collect-garbage)
    (let-values ([(val cpu real gc) (time-apply proc args)])
      gc))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Actual testing functions
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  

  ;; Runs bench-proc (see below) against system. Used to give
  ;; a reference of the performance of running system relative
  ;; to the reference system
  ;; get-score: Void -> Nat
  (define (get-score)
    (inexact->exact (truncate (* 10000000 (/ (get-time-real bench-proc null) ref-real)))))
  

  ;; Prints the score in a nice way, explaining what that silly
  ;; number is
  ;; print-score: Void -> Void
  ;; Example: (print-score) => Your System Score is: 34064717
  (define (print-score)
    (printf "Please wait...\n")
    (printf "Your System Score is: ~v"
            (get-score)))
  

  ;; Runs the benchmark against the passed in function (with arguments)
  ;; Computes the system's score at run-time
  ;; benchmark: (X1 X2 ... -> Any) [X1 X2 ...] -> Void
  (define (benchmark proc . args)
    (printf "Please wait...\n")
    (printf "This function would run in approximately ~v milliseconds on the reference system.\n"
            (* (/ (foldr (lambda (x y) (+ (get-time-real proc args) y))
                         0 (build-list 5 identity)) 5 (get-score)) 10000000)))


  ;; Runs the benchmark without computing the score.
  ;; Uses passed in score instead
  ;; benchmark-with-score: Nat (X1 X2 ... -> Any) [X1 X2 ...] -> Void
  (define (benchmark-with-score score proc . args)
    (printf "This function would run in approximately ~v milliseconds on the reference system.\n"
            (* (/ (foldr (lambda (x y) (+ (get-time-real proc args) y))
                         0 (build-list 5 identity)) 5 score) 10000000)))
  
  
  ;; Test used for comparing performance levels against
  ;; bench-proc: Void -> Void
  (define (bench-proc)
    (void (length (filter (lambda (x) (char=? x #\A))
                          (reverse 
                           (string->list 
                            (list->string 
                             (reverse
                              (map (lambda (x) (if (char=? x #\C) #\A #\F))
                                   (string->list
                                    (list->string
                                     (remove #\C
                                             (map (lambda (x) (if (char=? x #\A) #\E #\C))
                                                  (build-list 10000000
                                                              (lambda (x)
                                                                (cond [(= (modulo x 32) 0) #\A]
                                                                      [(<= (modulo (* x 1000) 137) 3) #\B]
                                                                      [(= (modulo (floor
                                                                                   (/ (/ (* (modulo 
                                                                                             (* x 9871528) 269) 0.5)
                                                                                         0.5) 23)) 27) 1) #\C]
                                                                      [else #\D]))))))))))))))))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (provide help
           print-score
           benchmark
           benchmark-with-score))