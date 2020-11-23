#lang racket
;;(require test-engine/racket-tests)

;; ***********************************
;;   Davide Radaelli (20894137)
;;   CS 135 Fall 2020
;;   Redux Question
;; ***********************************

;; -------------------------
;; parity question

;; (parity str) Consumes a string where each contained char is either a 1 or 0,
;; returns 'odd if the number 1's in the str is odd, else 'even is returned.
;; Require: str is made up of 1's and 0's
;; Examples:
(check-expect (parity "110101") 'even)
(check-expect (parity "1110011") 'odd)
(check-expect (parity "1") 'odd)
(check-expect (parity "0") 'even)

;; parity : (listof Char) -> Bool
(define (parity str) 
  (local
    [;; Helper function is self-evident
     (define int-lst 
       (local
         [(define char-lst (string->list str))]
         (map (lambda (x) (cond [(char=? x #\0) 0] [else 1])) char-lst)))]
    (cond [(even? (foldl + 0 int-lst)) 'even] [else 'odd])))

;; Tests:
(check-expect (parity "11111") 'odd)
(check-expect (parity "000000") 'even)

;; -------------------------
;; replace-word question


;; (replace-word str-search str-replace lst) Consumes two strings and a list of
;; strings, and produces a new list where all occurences of the first string 
;; have been replaced by the second string.

;; replace-word: Str Str (listof Str) -> (listof Str)

;; Examples:
(check-expect (replace-word "exam" "assessment" '("content" "exam" "assignment"))
              '("content" "assessment" "assignment"))
(check-expect (replace-word "hi" "hey" (cons "hi" (cons "hi" 
(cons "hi" empty)))) (cons "hey" (cons "hey" (cons "hey" empty))))

(define (replace-word str-search str-replace lst) 
  (map (lambda (str) 
      (cond
        [(equal? str str-search) str-replace]
        [else str])) lst))

;; Tests
(check-expect (replace-word "blah" "N" (cons "this" (cons "sentence" (cons
"keeps" (cons "going" (cons "on" (cons "blah" (cons "blah" (cons "blah" empty)
)))))))
) (cons "this" (cons "sentence" (cons "keeps" (cons "going" (cons "on" (cons "N"
(cons "N" (cons "N" empty)))))))))

;; -------------------------
;; number factors question

;; (all-factors n) Consumes natural number n and produces a list of all natural
;; number x where 0 < x < n divides n evenly in ascending order.

;; all-factors: Nat -> listof(Nat)

;; Examples:
(check-expect
 (all-factors 30)
 (cons 1 (cons 2 (cons 3 (cons 5 (cons 6 (cons 10 (cons 15 empty))))))))
(check-expect (all-factors 2) (cons 1 empty))

(define (all-factors n) 
  (local
    [(define (mod n) (lambda (x) (zero? (remainder n x))))]
    (filter (mod n) 
          (rest (build-list n (lambda (x) x))))))

;; Tests
(check-expect
 (all-factors 30)
 (cons 1 (cons 2 (cons 3 (cons 5 (cons 6 (cons 10 (cons 15 empty))))))))

(check-expect (all-factors 2) (cons 1 empty))

;; -------------------------
;; mean relative question

;; (mean-relative lst) Consumes a list of integers and produces a list of
;; symbols where each symbol in the produced list should be either 
;; 'below-mean, 'above-mean or 'mean

;; mean-relative : listof(Nat) -> listof(Sym)
;; Examples:
(check-expect (mean-relative '(5 7 9 12))
              '(below-mean below-mean above-mean above-mean))
(check-expect (mean-relative '(1 1 1 12))
              '(below-mean below-mean below-mean above-mean))

(define (mean-relative lst) 
  (local
    [(define sum (foldl + 0 lst))
     (define mean (/ sum (length lst)))]
    (map (lambda (x) 
           (cond
             [(> x mean) 'above-mean]
             [(< x mean) 'below-mean]
             [else 'mean])) lst)))

;; Tests
(check-expect (mean-relative '(1 1 1 1))
              '(mean mean mean mean))


;;(test)
