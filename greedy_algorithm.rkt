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
          (vector 0 0 1 2 0 2 0))); 6

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

; Get the value of a certain position
(define (ga_pos-value vec r c)
  (cond
    [(zero? (2d-vector-ref vec r c)) 0.25]   ; Empty position
    [(equal? 1 (2d-vector-ref vec r c)) 0]  ; Rival position
    [(equal? 2 (2d-vector-ref vec r c)) 1]  ; Own position
    [else 0]
    )
  )

; Get the value of all positions in a certain direction
(define (ga_dir-value vec r c d)
  (cond
    [(or (< (+ r (cadr d)) 0)
         (< (+ c (car d)) 0)
         (> (+ r (cadr d)) (- (vector-length vec) 1))
         (> (+ c (car d)) (- (vector-length (vector-ref vec 0)) 1))) 0]
    [(or (> (abs (car d)) 3)
         (> (abs (cadr d)) 3)
         (= (+ r (cadr d)) 0)
         (= (+ c (car d)) 0)
         (= (+ r (cadr d)) (- (vector-length vec) 1))
         (= (+ c (car d)) (- (vector-length (vector-ref vec 0)) 1)))
     (ga_pos-value vec (+ r (cadr d)) (+ c (car d)))]
    [(zero? (ga_pos-value vec (+ r (cadr d)) (+ c (car d)))) 0]
    [else (+ (ga_pos-value vec (+ r (cadr d)) (+ c (car d))) (ga_dir-value vec r c (list (+ (car d) (car d)) (+ (cadr d) (cadr d)))))]
    )
  )

(define (ga_total-value vec r c)
  (+ (ga_dir-value vec r c (list -1 0))
     (ga_dir-value vec r c (list 1 0))
     (ga_dir-value vec r c (list 0 -1))
     (ga_dir-value vec r c (list 0 1))
     (ga_dir-value vec r c (list -1 0))
     (ga_dir-value vec r c (list -1 -1))
     (ga_dir-value vec r c (list -1 1))
     (ga_dir-value vec r c (list 1 -1))
     (ga_dir-value vec r c (list 1 1))
     )
  )