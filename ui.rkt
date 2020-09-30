#lang racket/gui

(require (lib "graphics.ss" "graphics"))
(require "greedy_algorithm.rkt")
(open-graphics)

; Window settings
(define window (open-viewport "4Line" 1200 800))
((draw-solid-rectangle window) (make-posn 0 0) 1200 800 "darkgray")

; Draw the base of the board
(define (draw-board s x y c)
  (if (>= c s)
      ((draw-rectangle window) (make-posn 49 49) (* 50 s) (* 50 s) "black")
      (if (> x (* s 50))
          (draw-board s 50 (+ y 50) (+ c 1))
          (begin
            ((draw-ellipse window)(make-posn x y) 50 50 "blue")
            (draw-board s (+ x 50) y c)))))
; Call the function
(draw-board 16 50 50 0)

; Draw circles for the board
(define (draw-game vec r c)
  (cond
    [(>= r (vector-length vec)) (void)]
    [(>= c (vector-length (vector-ref vec 0))) (draw-game vec (+ r 1) 0)]
    [(equal? 1 (2d-vector-ref vec r c)) (begin ((draw-solid-ellipse window)(make-posn (+ 50 (* 50 c)) (+ 50 (* 50 r))) 50 50 "red") (draw-game vec r (+ c 1)))]
    [(equal? 2 (2d-vector-ref vec r c)) (begin ((draw-solid-ellipse window)(make-posn (+ 50 (* 50 c)) (+ 50 (* 50 r))) 50 50 "yellow") (draw-game vec r (+ c 1)))]
    [else (draw-game vec r (+ c 1))]))

(draw-game (test-matrix) 0 0)