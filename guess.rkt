#lang racket
;;(require test-engine/racket-tests)
(require "animals.rkt")

;; ***********************************
;;   Davide Radaelli (20894137)
;;   CS 135 Fall 2020
;;   Guess Filter Questions
;; ***********************************

;; -------------------------
;; collect-attributes question

;; (collect-attributes seen) Consumes a list of examples and produces a list of
;; attributes contained in the example with no duplicates
;; Examples:
(check-expect (collect-attributes '((animal t1 t2 t3))) '(t1 t2 t3))
(check-expect (collect-attributes '((animal t1 t2) (animal t1 t2) (animal t3))) '(t1 t2 t3))
(check-expect (collect-attributes '((goose large swims flies angry)
  (squirrel small)
  (crow medium flies angry)
  (duck small swims flies)
  (gull small flies angry)
  (sparrow small flies)
  (duck small swims flies)
  (goose large swims flies angry)
  (crow small flies angry)
  (gull small swims flies angry))) '(large swims flies angry small medium))

;; collect-attributes : (listof (listof Sym)) -> listof(Sym)
(define (collect-attributes seen) 
  (local
    [(define (collect-inner seen unique) 
      (local [;; (is-unique? unique) gives
              (define (my-special-filter unique lst) 
                (cond
                  [(empty? lst) empty]
                  [(match? unique (first lst)) (my-special-filter unique (rest lst))]
                  [else (cons (first lst) (my-special-filter unique (rest lst)))]))
              (define (match? unique sym) 
                (cond
                  [(empty? unique) false]
                  [else (or (symbol=? (first unique) sym) (match? (rest unique) sym))]))]
      (cond 
        [(empty? seen) unique]
        [else (collect-inner (rest seen) (append unique 
          (my-special-filter unique (rest (first seen)))))])))]
    (collect-inner seen empty)))

;; Tests:
(check-expect (collect-attributes '()) '())

;; -------------------------
;; split-examples question

;; (split-examples examples sym) Splits the list of examples on the given symbol.
;; Produces a list of two lists of examples, with the first containing the examples
;; containing the symbol and the second containing the examples not containing the
;; symbol
;; Examples:
(check-expect (split-examples '((animal t1 t2 t3)) 't1) '(((animal t1 t2 t3)) ()))
(check-expect (split-examples '((animal t1 t2) (animal t1 t2) (animal t3) (animal t4)) 't1) 
              '(((animal t1 t2) (animal t1 t2)) ((animal t3) (animal t4))))
(check-expect (split-examples '((goose large swims flies angry)
  (squirrel small)
  (crow medium flies angry)
  (duck small swims flies)
  (gull small flies angry)
  (sparrow small flies)
  (duck small swims flies)
  (goose large swims flies angry)
  (crow small flies angry)
  (gull small swims flies angry)) 'small) 
              '(((squirrel small)
  (duck small swims flies)
  (gull small flies angry)
  (sparrow small flies)
  (duck small swims flies)
  (crow small flies angry)
  (gull small swims flies angry)
  ) (
  (goose large swims flies angry)
  (crow medium flies angry)
  (goose large swims flies angry)
  )))

;; split-examples : (listof (listof Sym)) Sym -> 
;;                         (listof (listof (listof Sym)) (listof (listof Sym)))
(define (split-examples examples sym) 
  (local
    [
     (define (split-match examples sym) 
       (cond
         [(empty? examples) empty]
         [(match? (first examples) sym) (cons (first examples) (split-match (rest examples) sym))]
         [else (split-match (rest examples) sym)]))
     (define (split-not-match examples sym)
       (cond
         [(empty? examples) empty]
         [(match? (first examples) sym) (split-not-match (rest examples) sym)]
         [else (cons (first examples) (split-not-match (rest examples) sym))]))
     (define (match? animal sym) 
                (cond
                  [(empty? animal) false]
                  [else (or (symbol=? (first animal) sym) (match? (rest animal) sym))]))
     ]
    (list (split-match examples sym) (split-not-match examples sym))))

;; Tests:
(check-expect (split-examples '(()) 't) '(() (())))

;; -------------------------
;; histogram question

;; (histogram examples) Consumes a list of examples and produces a list of attribute
;; /count pairs, with each pair indicating how many times that attribute aprears in 
;; the examples.
;; Examples:
(check-expect (histogram '((animal t1 t2 t3))) '((t1 1) (t2 1) (t3 1)))
(check-expect (histogram '((animal t1 t2 t4) (animal t1 t2 t3) (animal t3) (animal t4)))
              '((t1 2) (t2 2) (t4 2) (t3 2)))
(check-expect (histogram '((goose large swims flies angry)
  (squirrel small)
  (crow medium flies angry)
  (duck small swims flies)
  (gull small flies angry)
  (sparrow small flies)
  (duck small swims flies)
  (goose large swims flies angry)
  (crow small flies angry)
  (gull small swims flies angry))) 
    '((large 2) (swims 5) (flies 9) (angry 6) (small 7) (medium 1)))

;; histogram : (listof (listof Sym)) -> (listof (list Sym Nat))
(define (histogram examples) 
  (local 
    [(define (histogram-inner attr-lst examples)
       (cond
         [(empty? attr-lst) empty]
         [else (cons (list (first attr-lst) (number-of (first attr-lst) examples)) 
                     (histogram-inner (rest attr-lst) examples))]))
     (define (number-of attr examples) (length (first (split-examples examples attr))))] 
    (histogram-inner (collect-attributes examples) examples)))

;; Tests:

;; -------------------------
;; augment-histogram question

;; (augment-histogram histogram attributes total) Consumes a histogram, a list
;; of all attributes and a total for the number of examples. Produces an 
;; augmented histogram which includes symbols with 0 instances in the example, 
;; as well as a count for the number of examples not containing the attribute.
;; Examples:
(check-expect
  (augment-histogram
    (list (list 'a 100) (list 'c 50))
    (list 'a 'b 'c)
    200)
  (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))
(check-expect
  (augment-histogram empty (list 'x 'y) 10)
  (list (list 'x 0 10) (list 'y 0 10)))

;; augment-histogram : (listof (list Sym Nat)) (listof Sym) Nat -> 
;;                                            (listof (list Sym Nat Nat)) 
(define (augment-histogram histogram attr-lst total) 
  (local 
    [(define (is-in? attr histogram) 
       (cond
         [(empty? histogram) false]
         [else (or (symbol=? attr (first (first histogram))) 
                   (is-in? attr (rest histogram)))]))
     (define (get-num attr histogram) 
       (cond
         [(symbol=? attr (first (first histogram))) (second (first histogram))]
         [else (get-num attr (rest histogram))]))]
    (cond
    [(empty? attr-lst) empty]
    [(is-in? (first attr-lst) histogram) 
     (cons (list (first attr-lst) (get-num (first attr-lst) histogram) 
                 (- total (get-num (first attr-lst) histogram))) 
     (augment-histogram histogram (rest attr-lst) total))]
    [else (cons (list (first attr-lst) 0 total)
                (augment-histogram histogram (rest attr-lst) total))])))

;; Tests:

;; -------------------------
;; entropy question

;; (entropy positive-counts negative-counts) Consumes two elements from augmented
;; histograms and produces their entropy. Both elements must be of the same attri
;; bute, and are taken fro, the augmented histogram for each split.
;; Examples:
(check-within (entropy (list 'large 126 59) (list 'large 146 669)) #i0.5663948489858 0.001)
(check-within (entropy (list 'small 17 168) (list 'large 454 361)) #i0.5825593868115 0.001)

;; entropy : (list Sym Nat Nat) (list Sym Nat Nat) -> Num
(define (entropy positive-counts negative-counts) 
  (local
    [(define a (second positive-counts))
     (define b (second negative-counts))
     (define c (third positive-counts))
     (define d (third negative-counts))
     (define (p-nm n m) 
       (cond
         [(zero? (+ n m)) 0.5]
         [else (/ n (+ n m))]))
     (define (e-p p) 
       (cond
         [(zero? p) 0]
         [else (* (- 0 p) (/ (log p) (log 2)))]))]
    (+ (* (p-nm (+ a b) (+ c d)) (+ (e-p (p-nm a b)) (e-p (p-nm b a)))) 
       (* (p-nm (+ c d) (+ a b)) (+ (e-p (p-nm c d)) (e-p (p-nm d c)))))))

;; Tests:
(check-within (entropy (list 'a 0 100) (list 'b 100 0)) #i0.0 0.001)


;; -------------------------
;; entropy-attributes question

;; (entropy-attributes positive negative) Consumes two augmented histograms and
;; computes the entropy of each attribute, producing a list of attribute/entropy
;; pairs.
;; Examples:
(check-within (entropy-attributes
  (list
   (list 'large 126 59) (list 'angry 161 24)
   (list 'small 17 168) (list 'flies 170 15)
   (list 'swims 162 23) (list 'medium 42 143))
  (list
    (list 'large 146 669) (list 'angry 469 346)
    (list 'small 454 361) (list 'flies 615 200)
    (list 'swims 365 450) (list 'medium 215 600)))
        (list
         (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
         (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
         (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)) 0.001)

;; entropy-attributes : (listof (list Sym Nat Nat)) (listof (list Sym Nat Nat)) -> 
;;                                   (listof (Sym Num))
(define (entropy-attributes positive negative) 
  (cond
      [(empty? positive) empty]
      [else (cons (list (first (first positive)) (entropy (first positive) (first negative))) 
                  (entropy-attributes (rest positive) (rest negative)))]))

;; Tests:
(check-within (entropy (list 'a 0 100) (list 'b 100 0)) #i0.0 0.001)

;; -------------------------
;; best-attributes question

;; (best-attributes entropies) Consumes a non-empty list of  attribute/entropy pa
;; irs. Produces the attribute with the minimum entropy.
;; Examples:
(check-expect (best-attributes (list
         (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
         (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
         (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))) 'large)
(check-expect (best-attributes (list (list 'a 1) (list 'b 2) (list 'c 3))) 'a)
(check-expect (best-attributes (list (list 'a 5) (list 'b 4) (list 'c 6))) 'b)

;; best-attributes : (listof (Sym Num)) -> Sym
(define (best-attributes lst) 
  (local
    [(define (best-inner lst best) 
       (cond
         [(empty? lst) (first best)]
         [(> (second (first lst)) (second best)) (best-inner (rest lst) best)]
         [else (best-inner (rest lst) (first lst))]))]
    (best-inner (rest lst) (first lst))))

;; Tests:

;; -------------------------
;; build-dt question

;; (best-attributes entropies) Consumes a non-empty list of  attribute/entropy pa
;; irs. Produces the attribute with the minimum entropy.
;; Examples:
(check-expect (best-attributes (list
         (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
         (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
         (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))) 'large)
(check-expect (best-attributes (list (list 'a 1) (list 'b 2) (list 'c 3))) 'a)
(check-expect (best-attributes (list (list 'a 5) (list 'b 4) (list 'c 6))) 'b)

;; best-attributes : (listof (Sym Num)) -> Sym
(define (best-attributes lst) 
  (local
    [(define (best-inner lst best) 
       (cond
         [(empty? lst) (first best)]
         [(> (second (first lst)) (second best)) (best-inner (rest lst) best)]
         [else (best-inner (rest lst) (first lst))]))]
    (best-inner (rest lst) (first lst))))

;; Tests:



;;(test)
