#lang racket/gui

; Test matrix
(define (test-matrix)
  ;               0 1 2 3 4 5 6 7
  (vector (vector 0 0 0 0 0 0 0 0)  ; 0
          (vector 0 0 0 0 0 0 0 0)  ; 1
          (vector 0 0 0 0 0 0 0 0)  ; 2
          (vector 0 0 0 0 0 0 0 0)  ; 3
          (vector 2 0 1 0 0 0 0 0)  ; 4
          (vector 1 0 1 0 0 2 2 0)  ; 5
          (vector 1 0 1 0 0 2 2 0)  ; 6
          (vector 1 2 1 0 0 2 2 0))); 7

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
    [(zero? (2d-vector-ref vec r c)) 0.1]    ; Empty position
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
    [(>= c (vector-length (vector-ref vec 0))) (ga_get-player p vec (+ r 1) 0)]
    [(equal? p (2d-vector-ref vec r c)) (append (list (list r c)) (ga_get-player p vec r (+ c 1)))]
    [else (ga_get-player p vec r (+ c 1))]))

; Checks a position and determinates if filling that position gives a solution
(define (ga_solution? vec r c lst)
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

; Get a position that would result in a solution for the given player
(define (ga_player-solution vec p c)
  (cond
    [(empty? c) '()]
    [(ga_solution? vec (car (car c)) (cadr (car c)) (ga_get-player p vec (car (car c)) (cadr (car c)))) (car c)]
    [else (ga_player-solution vec p (cdr c))]))

; Pick the position with the highest value
(define (ga_top-pos vec queue current)
  (cond
    [(empty? queue) current]
    [(> (ga_total-value vec (car (car queue)) (cadr (car queue))) (ga_total-value vec (car current) (cadr current))) (ga_top-pos vec (cdr queue) (car queue))]
    [else (ga_top-pos vec (cdr queue) current)]))

; Choose the best position to get a positive solution
(define (ga_choose vec)
  (cond
    [(not (empty? (ga_player-solution vec 2 (ga_get-C vec 0 0)))) (ga_player-solution vec 2 (ga_get-C vec 0 0))]
    [(not (empty? (ga_player-solution vec 1 (ga_get-C vec 0 0)))) (ga_player-solution vec 1 (ga_get-C vec 0 0))]
    [else (ga_top-pos vec (cdr (ga_get-C vec 0 0)) (car (ga_get-C vec 0 0)))]))