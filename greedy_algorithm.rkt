#lang racket/gui

; Instituto Tecnológico de Costa Rica
; Área Académica de Ingeniería en Computadores
; 
; Curso: CE3104 - Lenguajes, compiladores e intérpretes
; Grupo: 02
; Profesor: Marco Rivera Meneses
;
; Tarea #1: 4Line (with Greedy Algorithm)
;
; Programadores:  Víctor Ignacio Castrillo Muñoz - 2017110244 (Greedy Algorithm)
;                 Brayan Manuel Solano Fonseca
;                 Esteban Adolfo Morales Ureña

(require readline)

; ========== DATA STRUCTURES ==========

; A defined matrix for development testing
(define (test-matrix)
  ;               0 1 2 3 4 5 6 7
  (vector (vector 0 0 0 0 0 0 0 0)  ; 0
          (vector 0 0 0 0 0 0 0 0)  ; 1
          (vector 0 0 0 0 0 0 0 0)  ; 2
          (vector 0 0 0 0 0 0 0 0)  ; 3
          (vector 0 0 0 0 0 0 0 0)  ; 4
          (vector 0 0 0 0 0 0 2 0)  ; 5
          (vector 0 0 0 2 0 0 2 0)  ; 6
          (vector 1 0 0 2 2 1 1 1))); 7

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

; Converts a 2d vector to a list
(define (2d-vector->list vec r c l sl)
  (cond
    [(>= r (vector-length vec)) l]
    [(>= c (vector-length (vector-ref vec 0))) (2d-vector->list vec (+ r 1) 0 (append l (list sl)) '())]
    [else (2d-vector->list vec r (+ c 1) l (append sl (list (2d-vector-ref vec r c))))]))

; Converts a list to a 2d vector
(define (list->2d-vector lst mat)
  (cond
    [(empty? lst) (list->vector mat)]
    [else (list->2d-vector (cdr lst) (append mat (list (list->vector (car lst)))))]))

; Add a value to the bottom of a column
(define (add-to-column vec r c p)
  (cond
    [(is-bottom? vec r c)
       (list->2d-vector (list-set (2d-vector->list vec 0 0 '() '()) r (list-set (list-ref (2d-vector->list vec 0 0 '() '()) r) c p)) '())]
    [else (add-to-column vec (+ r 1) c p)]))

; ========== GREEDY ALGORITHM ==========

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

; Get all positions with the value of 2
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

; ========== GAME ==========

; Display the board in the console
(define (show-m vec r)
  (cond
    [(>= r (vector-length vec)) (void)]
    [else (display (vector-ref vec r))
          (display "\n")
          (show-m vec (+ r 1))]))

; Winning condition
(define (winner? vec)
  #f
  )

; Gameloop
(define (start-game vec t)
  (cond
    [(winner? vec) (display "Game over...\n")]
    [(odd? (remainder t 2))
        (show-m vec 0)
        (display "Choose the column you want to play...\n")
        (define input (string->number (read-line (current-input-port) 'any)))
        (start-game (add-to-column vec 0 input 1) (+ t 1))]
    [else
        (start-game (add-to-column vec 0 (cadr (ga_choose vec)) 2) (+ t 1))]))

; Set a new game
(define (set-game)
  (display "4Line\nPlease write the size of the board from 8 to 16...  ")
  (define input (string->number (read-line (current-input-port) 'any)))
  (if (and (>= input 8) (<= input 16))
      (start-game (make-vector input [make-vector input]) 1)
      (set-game)))

; Used to share this functions with the UI file
(provide (all-defined-out))