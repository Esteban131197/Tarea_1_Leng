#lang racket/gui

; Test matrix
(define (test-matrix)
  ;               0 1 2 3 4 5 6
  (vector (vector 0 0 0 0 0 0 0)  ; 0
          (vector 0 0 0 0 0 0 0)  ; 1
          (vector 0 0 0 0 0 0 0)  ; 2
          (vector 0 0 0 0 0 0 0)  ; 3
          (vector 0 0 0 0 0 0 0)  ; 4
          (vector 0 0 0 1 0 0 0)  ; 5
          (vector 0 0 2 1 0 2 0))); 6

; 2D vector
(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

; Check if a position in the 2D vector is last empty of the column
(define (is-bottom? vec r c)
  (cond
    [(not (zero? (2d-vector-ref vec r c))) #f]
    [(equal? r (- (vector-length vec) 1)) #t]
    [(not (zero? (2d-vector-ref vec (+ r 1) c))) #t]
    [else #f]))

; Get the list of possible positions to play
(define (ga_get-C vec r c)
  (cond
    [(>= r (vector-length vec)) '()]
    [(>= c (vector-length (vector-ref vec r))) (ga_get-C vec (+ r 1) 0)]
    [(and (zero? (2d-vector-ref vec r c)) (is-bottom? vec r c)) (append (list (list r c)) (ga_get-C vec r (+ c 1)))]
    [else (ga_get-C vec r (+ c 1))]))
