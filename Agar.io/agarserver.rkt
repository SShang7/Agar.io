#lang racket
(require 2htdp/universe 2htdp/image test-engine/racket-tests "sharedAgar.rkt")
(provide launch-server)
;(define out (open-output-file "test.txt" #:exists 'append))
(define (launch-server)
  (universe '()
            (on-new connect)
            (on-msg handle-msg)
            ))
;connect: serverstate iw -> bundle
(define (connect u client)
  (make-bundle
   (cons client u)
   (list (make-mail client 'welcome))
   empty))
;(check-expect (connect empty iworld1) (make-bundle (list iworld1) empty empty))
;(check-expect (connect (list iworld2 iworld3) iworld1)
;              (make-bundle (cons iworld1 (list iworld2 iworld3)) empty empty))

;replace : player aloi -> aloi
(define (replace p aloi)
  (cond
    [(empty? aloi) (cons p empty)]
    [else 
     (cons p (remove p aloi (lambda(a b)(symbol=? (iworld-name a) (iworld-name b)))))]))



;handle-msg: state iw msg -> bundle 
(define (handle-msg u client msg)
  (local ((define u2 (replace client u)))
    ;(writeln msg out)
     
    (cond
      [(empty? u) (make-bundle u empty empty)]
      [else
       (make-bundle
        u2
        (map (λ (iw) (make-mail iw msg)) u2)
        empty)])))

    
;(check-expect (handle-msg '((a 1)(b 2)(c 3)) '(a 1) '(a 4))
;              (make-bundle
;               '((a 4)(b 2)(c 3))
;               (map (lambda (iw) (make-mail iw '((a 1)(b 2)(c 3)))) '((a 1)(b 2)(c 3))) 
;               empty))
(test)