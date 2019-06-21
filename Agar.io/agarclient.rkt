#lang racket
(require 2htdp/universe 2htdp/image "sharedAgar.rkt")
(provide bang)
;replace : player alop -> alop
(define (replace p alop)
  (cond
    [(empty? alop) (cons p empty)]
    [else 
     (cons p (remove p alop (lambda(a b)(symbol=? (car a) (car b)))))]))
;bang : string string -> big-bang
(define (bang namer host)
  (define players '())
  (big-bang (world (list namer (player 10 (make-color (random 256) (random 256) (random 256))(random 1501) (random 801) 0 0))
                   (build-list 200 (lambda(n)(food (random-n-m 3 6)
                                                   (make-color (random 256) (random 256) (random 256))
                                                   (random 1501) (random 801))))
                   (build-list 7 (lambda(n)(virus (random 1501) (random 801))))
                   0 0)
            (to-draw create-scene)
            (on-mouse ...)
            (on-tick ...)
            (on-receive (λ (w msg) 
                          (cond 
                            [(symbol? msg) w]
                            [else
                             (set! players (replace msg players))
                             (if (symbol=? (car msg) namer) msg w)
                             ])))
            (on-name namer)
            (register host)))