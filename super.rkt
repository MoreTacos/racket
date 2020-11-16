#lang racket
;;(require test-engine/racket-tests)

;; ***********************************
;;   Davide Radaelli (20894137)
;;   CS 135 Fall 2020
;;   Super Filter Questions
;; ***********************************

;; -------------------------
;; super-filter question

;; (super-filter pred? lst) Produces list with elements lst for which the predi
;; cate is true. Works on nested lists.
;; Examples:
(check-expect (super-filter odd? '(1 2 3 4 5 (1 2 3) 6 7)) '(1 3 5 (1 3) 7))
(check-expect (super-filter odd? '((1 2 3))) '((1 3)))
(check-expect (super-filter odd? empty) empty)

;; super-filter : (Any -> Bool) (listof Any) -> listof(Any)
(define (super-filter pred? lst) (cond
    [(empty? lst) empty]
    [(list? (first lst)) (cons (super-filter pred? (first lst)) (super-filter pred? (rest lst)))]
    [(pred? (first lst)) (cons (first lst) (super-filter pred? (rest lst)))]
    [else (super-filter pred? (rest lst))]))

;; Tests:
(check-expect (super-filter odd? '(2 3 4 (1 2 3) 6 7)) '(3  (1 3) 7))

;; -------------------------
;; ruthless question

;; (ruthless lst) Takes in a potentially nested list of symbols and removes all 
;; instances of 'ruth from it.
;; Examples:
(check-expect
  (ruthless
    (list 'rabbit
          (list 'apple 'pluto
                (list 'ruth 'blue) 'ruth) 'hello))
  (list 'rabbit
        (list 'apple 'pluto
              (list 'blue)) 'hello))
(check-expect
  (ruthless
    (list 'ruth
          (list 'ruth 'ruth
                (list 'ruth 'ruth) 'ruth) 'ruth))
  '((())))
(check-expect (ruthless empty) empty)

;; ruthless: (listof Any) -> (listof Any)
(define (ruthless lst) (local
  [(define (is-ruth? s) (not (symbol=? s 'ruth)))] (super-filter is-ruth? lst)))

;; Tests:
(check-expect
  (ruthless
    (list 'rabbit
          (list 'apple 'ruth
                (list 'ruth 'blue) 'ruth) 'hello))
  (list 'rabbit
        (list 'apple
              (list 'blue)) 'hello))

;; -------------------------
;; supersize question

;; (supersize n lst) Takes in a potentially nested list of numbers and removes all 
;; numbers smaller then n
;; Examples:
(check-expect (supersize 4 (list 8 1 (list 2 6 3) 10 1))
              (list 8 (list 6) 10))
(check-expect (supersize 4 (list 3 1 (list 2 3 3) 0 1))
              (list (list)))
(check-expect (supersize 4 empty) empty)

;; supersize: (listof Any) -> (listof Any)
(define (supersize num lst) (local
  [(define (larger? n) (> n num))] (super-filter larger? lst)))

;; Tests:
(check-expect (supersize 0 (list 8 1 (list 2 6 3) 10 1))
              (list 8 1 (list 2 6 3) 10 1))

;; -------------------------
;; super-keeper question

;; (super-keeper pred? lst) Produces a list with the elements of lst for which
;; the predicate is false
;; Examples:
(check-expect
  (super-keeper
    odd?
    (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
  (list (list 2 (list 2 4) 6 (list 8)) 10 12))
(check-expect
  (super-keeper
    odd?
    (list 1 (list 1 (list 1 1 1) 1 1 (list 1 1 1)) 1 1 1))
  (list (list (list) (list))))
(check-expect
  (super-keeper
    odd?
    empty)
  empty)
;; super-keeper: (Any -> Bool) (listof Any) -> (listof Any)
(define (super-keeper pred? lst) (local
  [(define (npred? x) (not (pred? x)))] (super-filter npred? lst)))

;; Tests:
(check-expect
  (super-keeper
    odd?
    (list 7 (list 8 (list 0 4 8) 1 10 (list 1 2 1)) 2 1 2))
  (list (list 8 (list 0 4 8) 10 (list 2)) 2 2))

;;(test)
