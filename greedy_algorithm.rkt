#lang racket/gui

; Test matrix
(define (test-matrix)
  ;               0 1 2 3 4 5 6
  (vector (vector 0 0 0 0 0 0 0)  ; 0
          (vector 0 0 0 0 0 0 0)  ; 1
          (vector 0 0 0 0 0 0 0)  ; 2
          (vector 0 0 0 0 0 0 0)  ; 3
          (vector 2 0 1 0 0 0 0)  ; 4
          (vector 1 0 1 0 0 0 0)  ; 5
          (vector 1 2 1 0 0 2 2))); 6

; 2D vector
(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

; Check if an element is in a list
(define (member? lst elemt)
  (cond
    [(empty? lst) #f]
    [(equal? elemt (car lst)) #t]
    [else (member? (cdr lst) elemt)]))

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
    [(equal? 1 (2d-vector-ref vec r c)) 0]   ; Rival position
    [(equal? 2 (2d-vector-ref vec r c)) 1])) ; Own position

; Get the value of all positions in a certain direction
(define (ga_dir-value vec r c d m a)
  (cond
    [(or (> m 3)
         (< (+ r (* m (cadr d))) 0)
         (< (+ c (* m (car d))) 0)
         (>= (+ r (* m (cadr d))) (vector-length vec))
         (>= (+ c (* m (car d))) (vector-length (vector-ref vec 0)))
         (zero? (ga_pos-value vec (+ r (* m (cadr d))) (+ c (* m (car d))))))
     a]
    [else (ga_dir-value vec r c d (+ m 1) (+ a (ga_pos-value vec (+ r (* m (cadr d))) (+ c (* m (car d))))))]))

; Get the value of all positions in every direction
(define (ga_total-value vec r c)
  (+ (ga_dir-value vec r c (list 1 0) 1 0)
     (ga_dir-value vec r c (list -1 0) 1 0)
     (ga_dir-value vec r c (list 0 1) 1 0)
     (ga_dir-value vec r c (list 0 -1) 1 0)
     (ga_dir-value vec r c (list 1 1) 1 0)
     (ga_dir-value vec r c (list -1 -1) 1 0)
     (ga_dir-value vec r c (list 1 -1) 1 0)
     (ga_dir-value vec r c (list -1 1) 1 0)))

; Get all positions with a 2
(define (ga_get-player p vec r c)
  (cond
    [(>= r (vector-length vec)) '()]
    [(>= c (vector-length (vector-ref vec 0))) (ga_get-2 vec (+ r 1) 0)]
    [(equal? p (2d-vector-ref vec r c)) (append (list (list r c)) (ga_get-2 vec r (+ c 1)))]
    [else (ga_get-player p vec r (+ c 1))]))

; Checks a position and determinates if filling that position gives a solution
(define (ga-solution? vec r c lst)
  (cond
    [(or
      ; Horizontal
      (equal? 3(ga_dir-value vec r c (list 1 0) 1 0))
      (and (member? lst (list r (+ c -1))) (member? lst (list r (+ c 1))) (member? lst (list r (+ c 2))))
      (and (member? lst (list r (+ c -2))) (member? lst (list r (+ c -1))) (member? lst (list r (+ c 1))))
      (equal? 3 (ga_dir-value vec r c (list -1 0) 1 0))
      ; Vertical
      (equal? 3 (ga_dir-value vec r c (list 0 1) 1 0))
      (and (member? lst (list (+ r -1) c)) (member? lst (list (+ r 1) c)) (member? lst (list (+ r 2) c)))
      (and (member? lst (list (+ r -2) c)) (member? lst (list (+ r -1) c)) (member? lst (list (+ r 1) c)))
      (equal? 3 (ga_dir-value vec r c (list 0 -1) 1 0))
      ; Diagonal-R
      (equal? 3 (ga_dir-value vec r c (list 1 1) 1 0))
      (and (member? lst (list (+ r -1) (+ c -1))) (member? lst (list (+ r 1) (+ c 1))) (member? lst (list (+ r 2) (+ c 2))))
      (and (member? lst (list (+ r -2) (+ c -2))) (member? lst (list (+ r -1) (+ c -1))) (member? lst (list (+ r 1) (+ c 1))))
      (equal? 3 (ga_dir-value vec r c (list -1 -1) 1 0))
      ; Diagonal-L
      (equal? 3 (ga_dir-value vec r c (list -1 1) 1 0))
      (and (member? lst (list (+ r -1) (+ c 1))) (member? lst (list (+ r 1) (+ c -1))) (member? lst (list (+ r 2) (+ c -2))))
      (and (member? lst (list (+ r -2) (+ c 2))) (member? lst (list (+ r -1) (+ c 1))) (member? lst (list (+ r 1) (+ c -1))))
      (equal? 3 (ga_dir-value vec r c (list 1 -1) 1 0)))
     #t]
    [else #f]))
