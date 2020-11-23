#lang racket
;;(require test-engine/racket-tests)

;; ***********************************
;;   Davide Radaelli (20894137)
;;   CS 135 Fall 2020
;;   Super foldr Question
;; ***********************************

;; -------------------------
;; super-foldr question

;; (super-foldr f base lst) Consumes a function to fold, a base and a potentially
;; nested list. Folds the list recursively.
;; Examples:
(check-expect (super-foldr + 0 '(1 (5 5 (1 3)) (10 2) 2)) 29)
(check-expect (super-foldr - 0 '(1 (5 5 (1 3)) (10 2) 2)) 9)
(check-expect (super-foldr + 0 '(1 2 3)) 6)

;; super-foldr : (Any Any -> Any) Any (listof Any) -> Any
(define (super-foldr f base lst) 
  (local
    [(define (flatten lst) 
       (cond
         [(empty? lst) empty]
         [(list? (first lst)) (cons (super-foldr f base (first lst)) (flatten (rest lst)))]
         [else (cons (first lst) (flatten (rest lst)))]))]
    (foldr f base (flatten lst))))

;; Tests:
(check-expect (super-foldr + 0 '(1 (1 1) 3)) 6)

;; -------------------------
;; magnitudes question

;; (magnitudes nl) Produces the sum of the absolute values of the numbers in a 
;; nested list
;; Examples:
(check-expect (magnitudes '(1 (-5 -5 (1 -3)) (-10 2) 2)) 29)
(check-expect (magnitudes '(1)) 1)

;; magnitudes : (listof Any) -> Num
(define (magnitudes nl) 
  (super-foldr (lambda (x y) (+ (abs x) y)) 0 nl))

;; Tests:
(check-expect (magnitudes '(-1 -2 -3)) 6)

;; -------------------------
;; super-filter question

;; (super-filter f lst) Filters the nested list recursively using super-foldr
;; Examples:
(check-expect (super-filter odd? '(1 (2 (2 3 4) 5 6 (7 8 9)) 10 11 12))
              '(1 ((3) 5 (7 9)) 11))
(check-expect (super-filter odd? '(1 2 3 4 5 (1 2 3) 6 7)) '(1 3 5 (1 3) 7))
(check-expect (super-filter odd? '((1 2 3))) '((1 3)))
(check-expect (super-filter odd? '(1 2 3)) '(1 3))

;; super-filter : (Any -> Bool) (listof Any) -> (listof Any)
(define (super-filter f nl)
  (super-foldr (lambda (x y) 
      (cond [(list? x) (cons x y)] 
            [(f x) (cons x y)]
            [else y])) empty nl))

;; Tests:
(check-expect (super-filter odd? empty) empty)
(check-expect (super-filter odd? '(2 3 4 (1 2 3) 6 7)) '(3  (1 3) 7))


;;(test)
