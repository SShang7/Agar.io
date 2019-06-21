#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(struct player (name size color x y vx vy) #:mutable #:prefab)
(struct food (size color x y) #:mutable #:prefab)
(struct virus (x y)#:mutable #:prefab)
(struct world (player alof alov targetx targety) #:mutable #:prefab)
(struct posn (x y)#:mutable #:prefab)
(display "Username: ")
(define username (symbol->string (read)))
;create-scene : world  -> scene
(define (create-scene W)
  (local (
          (define (add-player player scene)
  (place-image
   (overlay/align
    "center"
    "center"
    (text (player-name player) (round(* 1/2 (player-size player))) "black")
    (overlay/xy (circle (player-size player) "outline" "black" ) 0 0
                (circle (player-size player) "solid" (player-color player))))
   (player-x player) (player-y player)
   scene))
          (define (add-foods L scene)
  (local ((define (add-food food scene)
           (place-image
            (circle  (food-size food) "solid" (food-color food))
            (food-x food) (food-y food)
            scene)))
    (foldl add-food scene L)))
          (define (add-viruses L scene)
  (local ((define virusimage (radial-star 55 50 55 "solid" "lime"))
          (define (add-virus virus scene)
            (place-image virusimage (virus-x virus) (virus-y virus) scene)))
    (foldl add-virus scene L))))
  (add-viruses (world-alov W)
               (add-player (world-player W)
                                          (add-foods (world-alof W) (empty-scene 1500 800))))))




;random-n-m : n n-> n
(define (random-n-m n m)
  (+ (random (- m n)) n))

;generate-food : -> food
(define (generate-food)
  (food (random-n-m 3 6)
        (make-color (random 256) (random 256) (random 256))
        (random 1500) (random 800)))




;mouse : world x y me -> world
(define (mouse w x y me)
  (cond
    [(string=? me "move")
     (world (world-player w)
                 (world-alof w)
                 (world-alov w)
                 x y)]
    [else w]))



;generate-virus : -> virus
(define (generate-virus)
  (virus (random 1501) (random 801)))



;;tock : World -> world
(define (tock W)
(local(

;distance : 4n -> n
(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))
       ;eat? : player food -> boolean
(define (eat? P F)
  (>= (player-size P) (distance (player-x P) (player-y P) (food-x F) (food-y F))))
;player-eat : player alof -> number
(define (player-eat P alof)
  (cond
    [(empty? alof) 0]
    [(cons? alof)
     (cond
       [(eat? P (first alof)) (+ .3 (player-eat P (rest alof)))]
       [else (player-eat P (rest alof))])]))





;eat-lof : player alof -> alof
(define (eat-lof P alof)
  (cond
    [(empty? alof)  empty]
    [(cons? alof)
     (cond
       [(eat? P (first alof)) (eat-lof P (cons (generate-food) (rest alof)))]
       [else (cons (first alof) (eat-lof P (rest alof)))])]))


;collide? : player virus -> boolean
(define (collide? P v)
  (and (< 50 (player-size P))
       (< (distance (player-x P) (player-y P)
                    (virus-x v) (virus-y v)) (player-size P))))

;collision-virus : player alov -> number
(define (collision-virus P alov)
  (cond
    [(empty? alov) 0]
    [(cons? alov)
     (cond
       [(collide? P (first alov)) (+ (* -3/8 (player-size P)) (collision-virus P (rest alov)))]
       [else (collision-virus P (rest alov))])]))


;virus-pop : player alov -> alov 
(define (virus-pop P alov)
  (cond
    [(empty? alov)  empty]
    [(cons? alov)
     (cond
       [(collide? P (first alov)) (virus-pop P (cons (generate-virus) (rest alov)))]
       [else (cons (first alov) (virus-pop P (rest alov)))])]))
 
;distance-0 : posn -> n
(define (distance-0 posn)
  (sqrt
   (+ (sqr (posn-x posn))
      (sqr (posn-y posn)))))



;newvelocity : px py tx ty -> (posn vx vy)
(define (newvelocity px py tx ty speed)
  (local ((define v(posn  (- tx px)(- ty py)))
          (define mag (distance-0 v)))
    (posn (* speed (/ (posn-x v) mag)) (* speed (/ (posn-y v) mag)))))


;newplayer : Player posn -> Player
;P' = P + dir/distance * speed 
;speed = 1 / (player-size P)
(define (newplayer P posn)
  (local ((define dist (distance-0  posn)))
    (player 
     (player-name P)
     (player-size P)
     (player-color P)
     (player-x P)
     (player-y P)
     (posn-x (newvelocity (player-x P) (player-y P) (posn-x posn) (posn-y posn) (/ 100 (player-size P))))              
     (posn-y (newvelocity (player-x P) (player-y P) (posn-x posn) (posn-y posn) (/ 100 (player-size P))))
     )))

;;change : World -> world
(define (change W)
  (world (player(player-name (world-player W))
                (player-size (world-player W))
               (player-color (world-player W))
               (+ (player-x (world-player W)) (player-vx (world-player W))) 
               (+ (player-y (world-player W)) (player-vy (world-player W))) 
               (player-vx (world-player W)) 
               (player-vy (world-player W)))
              (world-alof W)
              (world-alov W)
              (world-targetx W)
              (world-targety W)))

;;masslost : number -> number
(define (masslost n)
  (cond
    [(> n 40) (* n 0.9999)]
    [else n]))
)
  (change
   (world
    (newplayer 
     (player 
     (player-name (world-player W))
     (masslost (+ (player-size (world-player W))
         (player-eat (world-player W)  (world-alof W))
         (collision-virus (world-player W)  (world-alov W))))
      (player-color (world-player W))
      (player-x (world-player W))
      (player-y (world-player W))
      (player-vx (world-player W))
      (player-vy (world-player W)))
     (posn
      (world-targetx W)
      (world-targety W)))
    (eat-lof (world-player W) 
             (world-alof W))
    (virus-pop (world-player W)
               (world-alov W))
               
    
    (world-targetx W)
    (world-targety W)))))


(void (big-bang
 (world
  (player username 10 (make-color (random 256) (random 256) (random 256))(random 1501) (random 801) 0 0)
  (build-list 200 (lambda(n)(food (random-n-m 3 6)
        (make-color (random 256) (random 256) (random 256))
        (random 1501) (random 801))))
  (build-list 7 (lambda(n)(virus (random 1501) (random 801))))
  0 0)  
 (to-draw create-scene)
 (on-mouse mouse)
 (on-tick tock)
 (name "Agar.io")))