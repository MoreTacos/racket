#lang racket
;;(require test-engine/racket-tests)

;; ***********************************
;;   Davide Radaelli (20894137)
;;   CS 135 Fall 2020
;;   Matches Question
;; ***********************************

;; -------------------------
;; matches-func? question

;; (matches-func? f lst) Consumes a function and a list of pairs. Produces true
;; if each pair in the list is of the form (x (f x)), and false otherwise
;; Examples:
(check-expect (matches-func? sqr '((5 25) (2 4) (10 100))) true)
(check-expect (matches-func? add1 '((1 2) (3 5) (10 15))) false)
(check-expect (matches-func? even? '((1 #f) (2 #t) (3 #f))) true)

;; matches-func? : (Any -> Any) (list Any Any) -> Bool
(define (matches-func? f lst) 
  (foldl (lambda (x y) (and x y)) 
         true 
         (map (lambda (x) (equal? (f (first x)) (second x))) lst)))

;; Tests:
(check-expect (matches-func? (lambda (x) (+ x 2)) '((1 3) (2 4) (3 5))) true)

;;(test)
