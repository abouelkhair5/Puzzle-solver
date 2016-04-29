;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname neighbours) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (neighbours state low)
  (local [(define best-wpos (first (sort-wpos (state-position state))))
          (define best-wpos-loc (extract-wpos (state-grid state) best-wpos))]
    (cond [(fit? best-wpos-loc (first low))
           (cons (make-state (replace-wpos (state-grid state)
                                           best-wpos
                                           (string->list (first low)))
                             (remove best-wpos (state-positions state))
                             (remove (first low) (state-words state)))
                 (neighbours state (rest low)))]
          [else (neighbours state (rest low))])))