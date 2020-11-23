#lang racket
;;(require test-engine/racket-tests)

;; ***********************************
;;   Davide Radaelli (20894137)
;;   CS 135 Fall 2020
;;   ALF Question
;; ***********************************

;; -------------------------
;; occurrences question

;; (occurrences num lst) Consumes a number and a list of numbers, and produces
;; the number of times that the given number occurs in the list of nums
;; Examples:
(check-expect (occurrences 2 '(1 2 1 2 1 2 1 2)) 4)
(check-expect (occurrences 2 '(2 2 2 2 2 2 2 2)) 8)
(check-expect (occurrences 2 '(1 1 1 1 1 1 1 1)) 0)

;; occurrences : Nat (listof Nat) -> Nat
(define (occurrences num lst) 
  (length (filter (lambda (x) (equal? num x)) lst)))

;; Tests:
(check-expect (occurrences 2 empty) 0)

;; -------------------------
;; zip question

;; (zip lst1 lst2) Conmumes two lists of equal length, and produces a list
;; of pairs between elements in the two lists
;; Require: length of lst1 is equal to length of lst2
;; Examples:
(check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))
(check-expect (zip '(1) '(a)) '((1 a)))
(check-expect (zip '(1 2 3) '(1 2 3)) '((1 1) (2 2) (3 3)))
;; zip : (listof Any) (listof Any) -> (list Any Any)
(define (zip lst1 lst2) 
  (map (lambda (x y) (list x y)) lst1 lst2))

;; Tests:
(check-expect (zip empty empty) empty)

;; -------------------------
;; unzip question

;; (upzip zipped) Consumes a list of pairs and produces a list of two list.
;; Examples:
(check-expect (unzip '((1 a) (2 b) (3 c))) '((1 2 3) (a b c)))
(check-expect (unzip '((1 a))) '((1) (a)))
(check-expect (unzip '((1 1) (2 2) (3 3))) '((1 2 3) (1 2 3)))

;; unzip : (listof (list Any Any)) -> (list (listof Any) (listof Any))
(define (unzip zipped) 
  (list (map (lambda (x) (first x)) zipped) (map (lambda (x) (second x)) zipped)))

;; Tests:
(check-expect (unzip empty) (list empty empty))

;; -------------------------
;; subsequence question

;; (subsequence lst from to) Consumes a list and to natural numbers. It
;; produces the subsequence from lst that begins at 'from' and end just
;; before 'to'
;; Examples:
(check-expect (subsequence '(a b c d e f g) 1 4) '(b c d))
(check-expect (subsequence '(a b c d e f g) 1 1) '())
(check-expect (subsequence '(a b c d) 0 400) '(a b c d))

;; subsequence : (listof Any) Nat Nat -> (listof Any)
(define (subsequence lst from to) 
  (map (lambda (x) (first x)) 
         (filter (lambda (x) (and (>= (second x) from) (< (second x) to))) 
            (map (lambda (x y) (list x y)) lst (build-list (length lst) (lambda (x) x))))))

;; Tests:
(check-expect (subsequence '(a b c d) 0 4) '(a b c d))

;;(test)
